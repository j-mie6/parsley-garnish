{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
module Parsley.LiftPlugin.LiftReplace (replaceLiftDicts, codeNameString) where

-- GHC API
import TcEvidence
import TcRnTypes
import TyCon      (tyConSingleDataCon)
import TyCoRep    (Type (..))
import Outputable
import MkCore
import CoreSyn
import Bag
import qualified Unique as GHC
import qualified THNames as GHC
-- ghc
import qualified Desugar as GHC
import qualified GHC hiding (exprType)
import qualified GhcPlugins as GHC
#if __GLASGOW_HASKELL__ < 810
import qualified HsExpr as Expr
#else
import qualified GHC.Hs.Expr as Expr
#endif

import qualified TcEvidence as GHC
import qualified TcRnMonad as GHC

import Control.Monad.IO.Class ( liftIO )
import Control.Monad
import Data.Generics ( everywhereM,  mkM, listify )

import Parsley.PluginUtils (lookupModule, lookupId)
import Parsley.LiftPlugin.Error (getErrTc, makeError)
import Parsley.LiftPlugin.Fake (getFakeDicts)

#if __GLASGOW_HASKELL__ >= 810
import GHC.Hs.Extension
noExt :: NoExtField
noExt = noExtField
#else
import GHC (noExt)
#endif

codeNameString :: String
codeNameString = "code"

-----------------------------------------------------------------------------
-- The source plugin which fills in the dictionaries magically.

replaceLiftDicts :: [GHC.CommandLineOption] -> GHC.ModSummary -> TcGblEnv -> TcM TcGblEnv
replaceLiftDicts _opts _sum tc_env = do
  hscEnv <- GHC.getTopEnv
  pluginModule <- lookupModule hscEnv "LiftPlugin"

  -- This is the identifier we want to give some magic behaviour
  codeFnName <- lookupId pluginModule codeNameString

  -- We now look everywhere for it and replace the `Lift` dictionaries
  -- where we find it.
  new_tcg_binds <- mkM ( rewriteLiftDict codeFnName ) `everywhereM` GHC.tcg_binds tc_env

  -- Check if there are any instances which remain unsolved
  checkUsages (GHC.tcg_ev_binds tc_env) new_tcg_binds
  return tc_env { GHC.tcg_binds = new_tcg_binds }

rewriteLiftDict :: GHC.Id -> Expr.LHsExpr GHC.GhcTc -> GHC.TcM ( Expr.LHsExpr GHC.GhcTc )
rewriteLiftDict pName e@(GHC.L _ (Expr.HsApp _ (GHC.L _ (Expr.HsWrap _ _ (Expr.HsVar _ (GHC.L _ v)))) body))
  | pName == v = mkM (repair body) `everywhereM` e
rewriteLiftDict _ e = return e

checkLiftable_var :: Expr.LHsExpr GHC.GhcRn -> GHC.Id -> TcM (Maybe GHC.CoreExpr)
-- Externally defined names
checkLiftable_var body var | GHC.isGlobalId var = Just <$> mkSplice body
-- Top-level names
checkLiftable_var body var = do
  env <- tcg_type_env <$> GHC.getGblEnv
  case GHC.lookupNameEnv env (GHC.idName var) of
    Just _ -> Just <$> mkSplice body
    Nothing -> return Nothing

checkLiftable :: Expr.LHsExpr GHC.GhcTc -> GHC.TcM (Maybe GHC.EvExpr)
checkLiftable e | Just e' <- wrapView e = checkLiftable e'
checkLiftable (GHC.L _ (Expr.HsVar _ v)) = checkLiftable_var (var_body $ GHC.getName v) (GHC.unLoc v)
checkLiftable (GHC.L _ (Expr.HsConLikeOut _ c)) = Just <$> mkSplice (var_body $ GHC.getName c)
checkLiftable _ = return Nothing

wrapView :: Expr.LHsExpr GHC.GhcTc -> Maybe (GHC.LHsExpr GHC.GhcTc)
wrapView (GHC.L l (Expr.HsWrap _ _ e)) = Just (GHC.L l e)
wrapView _ = Nothing

-- We take the body and then look for all the `Lift` dictionaries to
-- replace them by a special one which ignores the argument.
-- The only case we deal with currently is if the `body` is a simple
-- variable. This won't deal with polymorphic functions yet.
repair :: Expr.LHsExpr GHC.GhcTc -> GHC.EvExpr -> GHC.TcM (GHC.EvExpr)
repair expr e = do
  let e_ty = GHC.exprType e
      (ty_con, tys) = GHC.splitTyConApp e_ty
  if (ty_con `GHC.hasKey` GHC.liftClassKey)
#if __GLASGOW_HASKELL__ < 810
      && GHC.isFunTy (head tys)
#else
      -- GHC 8.10 introduced levity polymorphic Lift, so there is an extra rep
      && GHC.isFunTy (head (tail tys))
#endif
    then do
      mres <- checkLiftable expr
      case mres of
        Just evidence -> return $ mkLiftDictionary (tyConSingleDataCon ty_con) e_ty evidence
        Nothing -> do
          makeError expr
          -- Return e to keep going
          return e
    else return e

var_body :: GHC.Name -> Expr.LHsExpr GHC.GhcRn
var_body v = (GHC.noLoc (Expr.HsVar noExt  (GHC.noLoc v)))

--TODO: This needs to work for [||var||] as well as [|var|]
mkSplice :: Expr.LHsExpr GHC.GhcRn -> TcM CoreExpr
mkSplice body = do
  hs_env  <- GHC.getTopEnv
  -- [| var |]
  let e = GHC.noLoc $ Expr.HsTcBracketOut noExt  (Expr.TExpBr noExt  body) []

  ( _, mbe ) <- liftIO ( GHC.deSugarExpr hs_env e )

  case mbe of
    Nothing -> panic "failed" -- Not sure what situation this happens.
    Just core_e -> return core_e

-- | Construct the specialised dictionary which constructs `[|| foo ||]`
-- from `lift foo`
mkLiftDictionary :: GHC.DataCon -> Type -> CoreExpr -> CoreExpr
mkLiftDictionary dc ty splice = --pprTouch "dict" $!
  let lvar = GHC.mkTemplateLocal 0 ty
      liftImpl = mkCoreLams [lvar] splice
#if __GLASGOW_HASKELL__ >= 810
      -- 8.10 introduced a levity polymorphic Lift
      rep = head (snd (GHC.splitTyConApp ty))
      -- TODO: This is _so_ not right, but maybe it's fine because newtype?
      liftTypedImpl = mkCoreLams [lvar] splice
      args = [Type rep, Type ty, liftImpl, liftTypedImpl]
#else
      args = [Type ty, liftImpl]
#endif
  in mkCoreConApps dc args

---- Checking Usages
-- TODO: I think we could store the landmine usage from the Ct information
-- in the type checking plugin
checkUsages :: Bag EvBind -> GHC.LHsBinds GHC.GhcTc -> TcM ()
checkUsages (bagToList -> bs) binds = do
  let landmines = getFakeDicts bs
      -- TODO: Use synthesise here?
      go :: TcEvBinds -> Bool
      go (EvBinds _) = True
      go _ = False
      extract = concatMap (\(EvBinds b) -> getFakeDicts (bagToList b))
      landmines' = extract $ listify go binds
      -- Get all variable usages, we don't want any landmines to appear.
      all_vars :: EvExpr -> Bool
      all_vars (Var _) = True
      all_vars _ = False
      usages = map (\(Var v) -> v) (listify all_vars binds)
  mapM_ (checkMine usages) (landmines ++ landmines')

-- Check whether a mine appears in the program.
checkMine :: [GHC.EvVar] -> (GHC.EvVar, Int) -> TcM ()
checkMine uses (v, k) = when (v `elem` uses) (getErrTc k)