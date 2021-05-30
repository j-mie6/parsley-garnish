{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}
module Parsley.OverloadedSyntaxPlugin.Plugin (plugin) where

-- GHC API
import Plugins    (Plugin (..), defaultPlugin, purePlugin)
import TcRnTypes
import Outputable
import Bag
import PrelNames
#if __GLASGOW_HASKELL__ < 810
import HsDumpAst
#else
import GHC.Hs.Dump
#endif
import Panic

-- ghc
import qualified GHC hiding (exprType)
import qualified GhcPlugins as GHC
#if __GLASGOW_HASKELL__ < 810
import qualified HsExpr as Expr
#else
import qualified GHC.Hs.Expr as Expr
#endif
import qualified TcRnMonad as GHC

import Data.Generics ( everywhere, mkT, everywhereBut, mkQ )
import Data.List
import GHC.Generics
import Data.Function

import Parsley.PluginUtils (lookupModule, lookupName, lookupNames)
import qualified Parsley.LiftPlugin.LiftReplace as LiftPlugin (codeNameString)

#if __GLASGOW_HASKELL__ >= 810
import GHC.Hs.Extension
noExt :: NoExtField
noExt = noExtField
type NoExt = NoExtField
#else
import GHC (noExt, NoExt)
#endif

data Names a = Names {
    ifName, unconsName, lamName, letName, elimProdName, apName, overloadName :: a
  } deriving (Functor, Traversable, Foldable, Generic)

namesString :: Names String
namesString =
  Names
    { ifName = "_if"
    , unconsName = "_uncons"
    , lamName = "_lam"
    , letName = "_let"
    , elimProdName = "_elim_prod"
    , apName = "_ap"
    , overloadName = "overload"
    }

-- Maps a representative name to the ordering and constructors we expect
-- with their arity
caseTable :: GHC.NameEnv CaseRow
caseTable = GHC.mkNameEnv [(fst (head (caseInfo ci)), ci) | ci <- caseTableInfo ]

data CaseRow = CaseRow { _overloadCase :: (forall a . Names a -> a)
                       , caseInfo :: [(GHC.Name, Int)]
                       }

-- mkCaseRow enforces the ordering invariant
mkCaseRow :: (forall a . Names a -> a) -> [(GHC.Name, Int)] -> CaseRow
mkCaseRow sel info = CaseRow sel (sortBy (GHC.stableNameCmp `on` fst) info)

tuple2Name :: GHC.Name
tuple2Name = GHC.dataConName (GHC.tupleDataCon GHC.Boxed 2)

caseTableInfo :: [CaseRow]
caseTableInfo =
  [ mkCaseRow unconsName [(GHC.consDataConName, 2), (GHC.nilDataConName, 0)]
  , mkCaseRow elimProdName [(tuple2Name, 2)] ]

-- Plugin definitions
plugin :: Plugin
plugin = defaultPlugin { renamedResultAction = overloadedSyntax, pluginRecompile = purePlugin }

{-----------------------------------------------------------------------------
-  The parser plugin - implement our own overloaded syntax
-  --------------------------------------------------------------------------}

overloadedSyntax :: [GHC.CommandLineOption] -> TcGblEnv -> GHC.HsGroup GHC.GhcRn -> TcM (TcGblEnv, GHC.HsGroup GHC.GhcRn)
overloadedSyntax _opts tc_gbl_env rn_group = do
  hscEnv <- GHC.getTopEnv
  pluginModule <- lookupModule hscEnv "OverloadedSyntaxPlugin"
  liftPluginModule <- lookupModule hscEnv "LiftPlugin"
  namesName <- lookupNames pluginModule namesString
  codeFnName <- lookupName liftPluginModule LiftPlugin.codeNameString

  let new_group = everywhere (mkT (overload_guard namesName codeFnName)) rn_group
  return (tc_gbl_env, new_group)

pattern VarApp :: GHC.Name -> Expr.LHsExpr GHC.GhcRn -> Expr.LHsExpr GHC.GhcRn
pattern VarApp v e <- (GHC.unLoc -> Expr.HsApp _ (GHC.unLoc -> Expr.HsVar _ (GHC.unLoc -> v)) e)

is_dollar :: GHC.Name -> Bool
is_dollar = (== dollarName)

pattern DollarVarApp :: GHC.Name -> Expr.LHsExpr GHC.GhcRn -> Expr.LHsExpr GHC.GhcRn
pattern DollarVarApp v e <- (GHC.unLoc -> Expr.OpApp _ (GHC.unLoc -> Expr.HsVar _ (GHC.unLoc -> v)) (GHC.unLoc -> Expr.HsVar _ (is_dollar . GHC.unLoc -> True)) e)

-- Look for direct applications of `overload e` or `overload $ e` and then
-- perform the overloading.
overload_guard :: Names GHC.Name -> GHC.Name -> Expr.LHsExpr GHC.GhcRn -> Expr.LHsExpr GHC.GhcRn
overload_guard names@(Names {overloadName}) codeFnName old_e =
  case old_e of
    VarApp v e       | v == overloadName -> overload_scope names codeFnName e
    DollarVarApp v e | v == overloadName -> overload_scope names codeFnName e
    _                                    -> old_e

-- Now perform the overriding just on the expression in this scope.
overload_scope :: Names GHC.Name -> GHC.Name -> Expr.LHsExpr GHC.GhcRn
                                                 -> Expr.LHsExpr GHC.GhcRn
overload_scope names codeFnName e =
    let mkVar = GHC.noLoc . Expr.HsVar noExt  . GHC.noLoc
        namesExpr = fmap mkVar names
    in everywhereBut (mkQ False (check_code codeFnName)) (mkT (overloadExpr namesExpr)) e

-- Don't recurse into LiftPlugin code
check_code :: GHC.Name -> Expr.LHsExpr GHC.GhcRn -> Bool
check_code codeFnName (GHC.L _ (Expr.HsApp _exp (GHC.L _ (Expr.HsVar _ name)) _)) = GHC.unLoc name == codeFnName
check_code _          _                                                           = False

overloadExpr :: Names (Expr.LHsExpr GHC.GhcRn) -> Expr.LHsExpr GHC.GhcRn -> Expr.LHsExpr GHC.GhcRn
overloadExpr names@Names{..} le@(GHC.L l e) = go e
  where
    go (Expr.HsIf _ext _ p te fe) = foldl' GHC.mkHsApp ifName [p, te, fe]
    go (Expr.HsApp _exp e1 e2) = foldl' GHC.mkHsApp apName [e1, e2]
    go (Expr.HsLam {}) = GHC.mkHsApp lamName le
    go (Expr.HsLet _ binds let_rhs) =
      let (binder, rhs) = extractBindInfo names binds
          pats = [GHC.noLoc $ GHC.VarPat noExt  binder]
          body_lam = mkHsLam pats let_rhs
      in foldl' GHC.mkHsApp letName [rhs, body_lam]

    go (Expr.HsCase _ext scrut mg) =
      let res = caseDataCon names scrut mg
      in case res of
           Left err -> panic err
           Right expr -> expr

    go expr = GHC.L l expr

mkHsLam :: [GHC.LPat GHC.GhcRn] -> Expr.LHsExpr GHC.GhcRn -> Expr.LHsExpr (GHC.GhcRn)
mkHsLam pats body = mkHsLamGRHS pats (GHC.unguardedGRHSs body)

mkHsLamGRHS :: [GHC.LPat GHC.GhcRn] -> Expr.GRHSs GHC.GhcRn (GHC.LHsExpr GHC.GhcRn)
                                    -> Expr.LHsExpr (GHC.GhcRn)
mkHsLamGRHS [] (Expr.GRHSs { grhssGRHSs = grhss, grhssLocalBinds = (GHC.L _ (GHC.EmptyLocalBinds _)) }) =
  case grhss of
    [GHC.L _ grhs] -> case simpleGRHS grhs of
                Just e -> e
                Nothing -> panic "GRHS is not simple2"

    _ -> panic "GRHS is not simple"
mkHsLamGRHS pats grhs = body_lam
  where
    matches = GHC.mkMatchGroup GHC.Generated [mkGRHSMatch pats grhs]
    body_lam = GHC.noLoc $ GHC.HsLam noExt  matches

mkGRHSMatch :: (GHC.XCMatch p body ~ NoExt) =>
                     [GHC.LPat p]
                     -> Expr.GRHSs p body -> GHC.Located (Expr.Match p body)
mkGRHSMatch pats rhs = GHC.noLoc $ GHC.Match noExt  GHC.LambdaExpr pats rhs

{- Code for dealing with let -}

-- Get the binder and body of let
extractBindInfo :: Names (Expr.LHsExpr GHC.GhcRn) -> GHC.LHsLocalBinds GHC.GhcRn -> (GHC.Located GHC.Name, GHC.LHsExpr GHC.GhcRn)
extractBindInfo names (GHC.L _ (GHC.HsValBinds _ (GHC.XValBindsLR (GHC.NValBinds binds _)))) = getBinds binds
  where
    getBinds bs =
      let [rs] = map snd bs
      in
      case bagToList rs of
        [GHC.L _ (GHC.FunBind _ bid matches _ _)] ->
          case isSimpleMatchGroup matches of
            Just simple -> (bid, simple)
            _ -> (bid, overloadExpr names (GHC.noLoc $ Expr.HsLam noExt  matches))
        -- Not dealing with guards here yet but they could be
        -- transfered onto the lambda
        _ -> panic "abc"
extractBindInfo _ e = panicDoc "abc2" (showAstData BlankSrcSpan e)

simpleGRHS :: Expr.GRHS p w -> Maybe w
simpleGRHS grhs =
  case grhs of
    (Expr.GRHS _ [] body) -> Just body
    _ -> Nothing

-- A simple match group is one which is just a variable with no
-- arguments.
-- let x = y
isSimpleMatchGroup :: Expr.MatchGroup id body -> Maybe body
isSimpleMatchGroup (Expr.MG { mg_alts = matches })
  | [GHC.L _ match] <- GHC.unLoc matches
  , Expr.Match { m_grhss = GHC.GRHSs { grhssGRHSs = [rhs] }
               , m_pats = [] } <- match
  = simpleGRHS (GHC.unLoc rhs)
  | otherwise
  = Nothing
isSimpleMatchGroup (Expr.XMatchGroup _) = panic "unhandled"

{- Code for dealing with case -}
-- Look at a case and see if it's a simple match on a data con
caseDataCon :: Names (Expr.LHsExpr GHC.GhcRn)
            -> Expr.LHsExpr (GHC.GhcRn)
            -> Expr.MatchGroup (GHC.GhcRn) (Expr.LHsExpr (GHC.GhcRn))
            -> Either String (GHC.LHsExpr GHC.GhcRn)
caseDataCon names scrut (Expr.MG { mg_alts = (GHC.L _l alts) }) = do
  res <- sortBy (\(_, n, _) (_, n1, _)-> GHC.stableNameCmp n n1) <$>
              (mapM (extractConDetails . GHC.unLoc) $ alts)
  case res of
    [] -> Left "No patterns"
    ps@((_, n, _):_) ->
      case GHC.lookupNameEnv caseTable n of
        Nothing -> Left "Not able to overload this constructor"
        Just (CaseRow sel spec) ->
          let con = GHC.mkHsApp (sel names) scrut
          in checkAndBuild con ps spec
caseDataCon _ _ (Expr.XMatchGroup _) = panic "unhandled"

checkAndBuild :: Expr.LHsExpr GHC.GhcRn -- The constructor
              -> [([GHC.LPat GHC.GhcRn], GHC.Name
                                       , Expr.GRHSs GHC.GhcRn (GHC.LHsExpr GHC.GhcRn))]
              -> [(GHC.Name, Int)]
              -> Either String (Expr.LHsExpr GHC.GhcRn)
checkAndBuild con [] [] = Right con
checkAndBuild _con (_:_) [] = Left "Too many patterns in program"
checkAndBuild _con [] (_:_) = Left "Not enough patterns in program"
checkAndBuild con (p:ps) (s:ss) = do
  res <- checkAndBuild con ps ss
  let (pats, pat_con, rhs) = p
      (spec_con, arity) = s
  if | pat_con /= spec_con -> Left "Constructors do not match"
     | length pats /= arity -> Left "Arity does not patch"
     | otherwise -> Right (GHC.mkHsApp res (mkHsLamGRHS pats rhs))

-- Extract, binders, con name and rhs
extractConDetails :: Expr.Match GHC.GhcRn body
                  -> Either String ([GHC.LPat GHC.GhcRn], GHC.Name, Expr.GRHSs GHC.GhcRn body)
-- There should only be one pattern in a case
extractConDetails (Expr.Match { m_pats = [pat], m_grhss = rhs }) = do
  (vars, cn) <- extractFromPat pat
  return (vars, cn, rhs)
extractConDetails _ = Left "More than one match"


extractFromPat :: GHC.LPat GHC.GhcRn -> Either String ([GHC.LPat GHC.GhcRn], GHC.Name)
#if __GLASGOW_HASKELL__ == 808
-- Trees that Grow :)
extractFromPat (GHC.XPat (GHC.L _l p)) =
#else
extractFromPat (GHC.L _l p) =
#endif
  case p of
    GHC.ConPatIn (GHC.L _l n) con_pat_details
      -> Right (GHC.hsConPatArgs con_pat_details, n)
    GHC.ParPat _ pp -> extractFromPat pp
    GHC.TuplePat _ [a, b] GHC.Boxed ->
      Right ([a, b], tuple2Name)
    {-GHC.VarPat _ _ -> Left "Unexpected VarPat"
    GHC.WildPat _ -> Left "Unexpected WildPat"
    GHC.LazyPat _ _ -> Left "Unexpected LazyPat"
    GHC.AsPat _ _ _ -> Left "Unexpected AsPat"
    GHC.BangPat _ _ -> Left "Unexpected BangPat"
    GHC.ListPat _ _ -> Left "Unexpected ListPat"
    GHC.SumPat _ _ _ _ -> Left "Unexpected SumPat"
    GHC.ConPatOut _ _ _ _ _ _ _ -> Left "Unexpected ConPatOut"
    GHC.ViewPat _ _ _ -> Left "Unexpected ViewPat"
    GHC.SplicePat _ _ -> Left "Unexpected SplicePat"
    GHC.LitPat _ _ -> Left "Unexpected LitPat"
    GHC.NPat _ _ _ _ -> Left "Unexpected NPat"
    GHC.NPlusKPat _ _ _ _ _ _ -> Left "Unexpected NPlusKPat"
    GHC.SigPat _ _ _ -> Left "Unexpected SigPat"
    GHC.CoPat _ _ _ _ -> Left "Unexpected CoPat"
    GHC.XPat _ -> Left "Trees that Grow"-}
    _          -> Left "Unexpectedly Complex Pattern"
#if __GLASGOW_HASKELL__ == 808
extractFromPat _ = Left "Something isn't a tree that grows?"
#endif