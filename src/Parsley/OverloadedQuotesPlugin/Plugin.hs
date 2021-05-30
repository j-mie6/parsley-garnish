{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
module Parsley.OverloadedQuotesPlugin.Plugin where

import Plugins       (Plugin (..), defaultPlugin, purePlugin)
import TcRnTypes     (TcGblEnv, TcM)
import Data.Generics (mkT, everywhere)
import GHC.Generics  (Generic)

import qualified GHC               (HsGroup, GhcRn, Name, GenLocated(L))
import qualified GhcPlugins as GHC (CommandLineOption)
import qualified TcRnMonad as GHC  (getTopEnv)

#if __GLASGOW_HASKELL__ < 810
import qualified HsExpr as Expr
#else
import qualified GHC.Hs.Expr as Expr
#endif

import Parsley.PluginUtils (lookupModuleInPackage, lookupNames, pprTouch)

{-|
This plugin repurposes /Untyped/ Template Haskell quotes (and splices within them)
to be `Parsley.Quapplicative` values.
-}
plugin :: Plugin
plugin = defaultPlugin { renamedResultAction = overloadedQuotes, pluginRecompile = purePlugin }

data QOps a = QOps {
    _code :: a,
    _val :: a,
    makeQ :: a
  } deriving (Functor, Foldable, Traversable, Generic)

quapplicativeStrings :: QOps String
quapplicativeStrings = QOps {
    _code = "_code",
    _val  = "_val",
    makeQ = "makeQ"
  }

overloadedQuotes :: [GHC.CommandLineOption] -> TcGblEnv -> GHC.HsGroup GHC.GhcRn -> TcM (TcGblEnv, GHC.HsGroup GHC.GhcRn)
overloadedQuotes _ gEnv rn = do
  hscEnv <- GHC.getTopEnv
  parsley <- lookupModuleInPackage hscEnv "parsley" "Parsley"
  qops <- lookupNames parsley quapplicativeStrings
  pprTouch "names" [makeQ qops, _code qops, _val qops] `seq` return ()
  return (gEnv, everywhere (mkT (transformUTHQuote qops)) rn)

-- The goal here is to find [|e|] and turn it into makeQ e [||e||]
-- The catch is that for any $qe in the quote, it must be hoisted out, let bound and then re-incorporated
-- such that `[|x .. $qe .. y|]` ~> `let qe' = qe in makeQ (x .. _val qe' .. y) [||x .. $$(_code qe') .. y||]`
-- This means that the content of the splice _cannot_ use any of the free-variables defined within
-- the original quote. Perhaps in that case we could just inline the definition into both holes...
-- As `transform` works bottom up, we can always assume nested quotes are already handled: this might
-- get tricky, however.
transformUTHQuote :: QOps GHC.Name -> Expr.LHsExpr GHC.GhcRn -> Expr.LHsExpr GHC.GhcRn
transformUTHQuote _ (GHC.L s (Expr.HsRnBracketOut ex (Expr.ExpBr ex' x) _)) = pprTouch "new quote" $ GHC.L s (Expr.HsBracket ex (Expr.TExpBr ex' x))
transformUTHQuote _ (GHC.L s (Expr.HsSpliceE ex (Expr.HsUntypedSplice ex' d x y))) =  pprTouch "new splice" $ GHC.L s (Expr.HsSpliceE ex (Expr.HsTypedSplice ex' d x y))
transformUTHQuote _ x = x
