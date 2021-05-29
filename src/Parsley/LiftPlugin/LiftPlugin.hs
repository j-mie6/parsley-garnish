{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Parsley.LiftPlugin.LiftPlugin (plugin) where

-- external
import Data.Maybe          (mapMaybe)
import GHC.TcPluginM.Extra (lookupModule, lookupName)

-- GHC API
import FastString (fsLit)
import Module     (mkModuleName)
import OccName    (mkTcOcc)
import Plugins    (Plugin (..), defaultPlugin, purePlugin)
import TcEvidence
import TcPluginM  (tcLookupTyCon)
#if __GLASGOW_HASKELL__ < 810
import TcRnTypes
#else
import TcRnTypes
import Constraint
#endif
import TyCon      (TyCon, tyConSingleDataCon)
import TyCoRep    (Type (..))
import Outputable
import Class
import MkCore
import CoreSyn
import Bag
import TcErrors
import Literal
import PrelNames
import qualified Unique as GHC
import qualified THNames as GHC
-- ghc
import qualified Desugar as GHC
import qualified Finder as GHC
import qualified GHC hiding (exprType)
import qualified GhcPlugins as GHC
#if __GLASGOW_HASKELL__ < 810
import qualified HsExpr as Expr
#else
import qualified GHC.Hs.Expr as Expr
#endif
#if __GLASGOW_HASKELL__ >= 810
import qualified Predicate as GHC
import qualified TyCoRep as GHC
#endif

import qualified IfaceEnv as GHC
import qualified TcEvidence as GHC
import qualified TcRnMonad as GHC
import qualified TysPrim as GHC

import Control.Monad.IO.Class ( liftIO )
import Control.Monad
import Data.Generics ( everywhereM,  mkM, listify )
import GHC.Generics
import Data.IORef
import System.IO.Unsafe

#if __GLASGOW_HASKELL__ >= 810
import GHC.Hs.Extension
noExt :: NoExtField
noExt = noExtField
#else
import GHC (noExt)
#endif

-- In this IORef we store how we would have reported the error
ioRef :: IORef (Int, [TcM ()])
ioRef = unsafePerformIO (newIORef (0, []))
{-# NOINLINE ioRef #-}

addError :: TcM () -> IO Int
addError err = atomicModifyIORef ioRef (\(k, errs) -> ((k+1, errs ++ [err]), k))

getError :: Int -> IO (TcM ())
getError k = (!! k) . snd <$> readIORef ioRef

data Names a = Names
  { pureName :: a }
  deriving (Functor, Traversable, Foldable, Generic)

namesString :: Names String
namesString =
  Names
    { pureName = "code"
    }

-- Plugin definitions
plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const (Just liftPlugin)
                       , typeCheckResultAction = replaceLiftDicts
                       , pluginRecompile = purePlugin }

liftPlugin :: TcPlugin
liftPlugin =
  TcPlugin { tcPluginInit  = lookupLiftTyCon
           , tcPluginSolve = solveLift
           , tcPluginStop  = const (return ())
           }

-- TODO: This is going to need to be augmented with unsafeTExpCoerce/unsafeCodeCoerce
type LiftEnv = TyCon
lookupLiftTyCon :: TcPluginM LiftEnv
lookupLiftTyCon = do
    md      <- lookupModule liftModule liftPackage
    liftTcNm <- lookupName md (mkTcOcc "Lift")
    tcLookupTyCon liftTcNm
  where
    liftModule  = mkModuleName "Language.Haskell.TH.Syntax"
    liftPackage = fsLit "template-haskell"

-- This plugin solves all instances of (Lift (a -> b)) with a dummy value.
solveLift :: LiftEnv -- ^ Lift's TyCon
         -> [Ct]  -- ^ [G]iven constraints
         -> [Ct]  -- ^ [D]erived constraints
         -> [Ct]  -- ^ [W]anted constraints
         -> TcPluginM TcPluginResult
solveLift _     _ _ []      = return (TcPluginOk [] [])
solveLift liftTc _gs _ds wanteds = --pprTrace "solveGCD" (ppr liftTc $$ ppr wanteds $$ ppr solved) $
  do res <- mapM (\c -> (, c) <$> evMagic c) solved
     return $! case failed of
       [] -> TcPluginOk res []
       f  -> TcPluginContradiction f
  where
    liftWanteds :: [Ct]
    liftWanteds = mapMaybe (toLiftCt liftTc) wanteds

    solved, failed :: [Ct]
    (solved,failed) = (liftWanteds, [])

--pprTouch :: Outputable a => String -> a -> a
--pprTouch name x = pprTrace name (ppr x) x

toLiftCt :: TyCon -> Ct -> Maybe Ct
toLiftCt liftTc ct =
  case GHC.classifyPredType $ ctEvPred $ ctEvidence ct of
    GHC.ClassPred tc tys
     | classTyCon tc == liftTc
#if __GLASGOW_HASKELL__ < 810
     , [ty] <- tys
#else
     -- GHC 8.10 introduced levity polymorphic Lift, so there is an extra rep
     , [_rep, ty] <- tys
#endif
     , GHC.isFunTy ty
      -> Just ct
    _ -> Nothing

mkWC :: Ct -> WantedConstraints
mkWC ct = WC (unitBag ct) emptyBag

addErrTc :: TcM () -> TcPluginM Int
addErrTc err = unsafeTcPluginTcM (liftIO (addError err))
getErrTc :: Int -> TcM ()
getErrTc k = join (liftIO (getError k))

-- | TODO: Check here for (->) instance
evMagic :: Ct -> TcPluginM EvTerm
evMagic ct = do
  let reporter = reportAllUnsolved (mkWC ct)
  k <- addErrTc reporter
  return $ (EvExpr (FakeExpr (ctPred ct) k))

-- What this is doens't matter really
fakeIdName :: GHC.Name
fakeIdName = GHC.mkWiredInIdName gHC_MAGIC (fsLit "fake_id") fakeIdKey fakeId

fakeIdKey :: Unique
fakeIdKey = GHC.mkPreludeMiscIdUnique 109

fakeId :: GHC.Id
fakeId = GHC.mkVanillaGlobalWithInfo fakeIdName ty info
  where
    info = GHC.noCafIdInfo
#if __GLASGOW_HASKELL__ < 810
    ty = GHC.mkSpecForAllTys [GHC.alphaTyVar] (GHC.mkFunTy GHC.intTy GHC.alphaTy)
#else
    ty = GHC.mkSpecForAllTys [GHC.alphaTyVar] (GHC.mkFunTy GHC.VisArg GHC.intTy GHC.alphaTy)
#endif

fake_id :: GHC.Id
fake_id = fakeId

is_fake_id :: GHC.Id -> Bool
is_fake_id = (== fake_id)

fake_expr :: Type -> Int -> Expr GHC.Id
fake_expr ty k = mkCoreApps (Var fake_id) [Type ty, (Lit $ LitNumber LitNumInteger (fromIntegral k) GHC.intTy)]

pattern FakeExpr :: Type -> Int -> Expr GHC.Id
pattern FakeExpr ty k <- App (App (Var (is_fake_id -> True)) (Type ty)) (Lit (LitNumber LitNumInteger (fromIntegral -> k) _))
  where
    FakeExpr k = fake_expr k

-----------------------------------------------------------------------------
-- The source plugin which fills in the dictionaries magically.
lookupIds :: GHC.Module -> Names String -> TcM (Names GHC.Id)
lookupIds pm names = lookupNames pm names >>= traverse GHC.lookupId

lookupNames :: GHC.Module -> Names String -> TcM (Names GHC.Name)
lookupNames pm = traverse (GHC.lookupOrig pm . GHC.mkVarOcc)

replaceLiftDicts :: [GHC.CommandLineOption] -> GHC.ModSummary -> TcGblEnv -> TcM TcGblEnv
replaceLiftDicts _opts _sum tc_env = do
  hscEnv <- GHC.getTopEnv
  {-v <- liftIO (readIORef ioRef)-}

  GHC.Found _ pluginModule <-
    liftIO
      ( GHC.findImportedModule
          hscEnv
          ( GHC.mkModuleName "Parsley.LiftPlugin" )
          Nothing
      )

  -- This is the identifier we want to give some magic behaviour
  Names{..} <- lookupIds pluginModule namesString

  -- We now look everywhere for it and replace the `Lift` dictionaries
  -- where we find it.
  new_tcg_binds <-
     mkM ( rewriteLiftDict pureName ) `everywhereM` GHC.tcg_binds tc_env

  -- Check if there are any instances which remain unsolved
  checkUsages (GHC.tcg_ev_binds tc_env) new_tcg_binds

  return tc_env { GHC.tcg_binds = new_tcg_binds }

-- |
rewriteLiftDict :: GHC.Id -> Expr.LHsExpr GHC.GhcTc -> GHC.TcM ( Expr.LHsExpr GHC.GhcTc )
rewriteLiftDict pName
  e@( GHC.L _ ( Expr.HsApp _ ( GHC.L _ ( Expr.HsWrap _ _ ( Expr.HsVar _ ( GHC.L _ v ) ) ) ) body) )
    | pName == v
    = mkM (repair body) `everywhereM` e
rewriteLiftDict _ e =
 -- pprTrace "rewriteAssert" (ppr e $$ showAstData NoBlankSrcSpan e) $
   return e


checkLiftable_var :: Expr.LHsExpr GHC.GhcRn -> GHC.Id -> TcM (Maybe GHC.CoreExpr)
-- Externally defined names
checkLiftable_var body var
 | GHC.isGlobalId var = Just <$> mkSplice body
-- Top-level names
checkLiftable_var body var = do
  env <- tcg_type_env <$> GHC.getGblEnv
  case GHC.lookupNameEnv env (GHC.idName var) of
    Just _ -> Just <$> mkSplice body
    Nothing -> return Nothing

checkLiftable :: Expr.LHsExpr GHC.GhcTc -> GHC.TcM (Maybe GHC.EvExpr)
checkLiftable e | Just e' <- wrapView e = checkLiftable e'
checkLiftable (GHC.L _ (Expr.HsVar _ v)) = checkLiftable_var (var_body $ GHC.getName v) (GHC.unLoc v)
checkLiftable (GHC.L _ (Expr.HsConLikeOut _ c)) = do
  Just <$> mkSplice (var_body $ GHC.getName c)
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
      --res = ty_con `GHC.hasKey` GHC.liftClassKey
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
          --GHC.panicDoc "skipping" (ppr expr $$ showAstData BlankSrcSpan expr)
    else return e

makeError :: Expr.LHsExpr GHC.GhcTc -> GHC.TcM ()
makeError (GHC.L l e) =
  GHC.setSrcSpan l $
  GHC.addErrCtxt (text "In" <+> ppr e) $
  GHC.failWithTc msg
  where
    msg = text "Unable to magically lift the argument."
          <+> text "It probably isn't statically known."

{-
  mb_local_use <- GHC.getStageAndBindLevel (GHC.idName v)
  case mb_local_use of
    Just (top_level, bind_lvl, use_stage)
      | GHC.isNotTopLevel top_level
      -> GHC.panicDoc "error" (ppr v)
    _ -> return .
repair e = return e
-}

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

getFakeDicts :: [EvBind] -> [(GHC.EvVar, Int)]
getFakeDicts = mapMaybe getFakeDict
  where
    getFakeDict (EvBind r (EvExpr (FakeExpr _ k)) _) = Just (r, k)
    getFakeDict _ = Nothing