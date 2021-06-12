{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
module Parsley.OverloadedQuotesPlugin.Plugin (plugin) where

import Data.Generics (GenericT, GenericQ, mkT, mkQ, everywhere, gmapT)
import GHC.Generics  (Generic)

import Parsley.PluginUtils (lookupModule, lookupNames)

#if __GLASGOW_HASKELL__ >= 900
import GHC.Driver.Plugins       (Plugin (..), defaultPlugin, purePlugin)
import GHC.Tc.Types             (TcM, TcGblEnv)


import qualified GHC               (HsGroup, GhcRn, Name, GenLocated(L), SrcSpan)
import qualified GHC.Plugins as GHC (CommandLineOption)
import qualified GHC.Tc.Utils.Monad as GHC  (getTopEnv)
#else
import Plugins       (Plugin (..), defaultPlugin, purePlugin)
import TcRnTypes     (TcGblEnv, TcM)


import qualified GHC               (HsGroup, GhcRn, Name, GenLocated(L), SrcSpan)
import qualified GhcPlugins as GHC (CommandLineOption)
import qualified TcRnMonad as GHC  (getTopEnv)
#endif

#if __GLASGOW_HASKELL__ < 810
import qualified HsExpr as Expr
#else
import qualified GHC.Hs.Expr as Expr
#endif

#if __GLASGOW_HASKELL__ >= 810
import GHC.Hs.Extension
noExt :: NoExtField
noExt = noExtField
#else
import GHC (noExt)
#endif

type Expr = Expr.LHsExpr GHC.GhcRn

{-|
This plugin repurposes /Untyped/ Template Haskell quotes (and splices within them)
to be `Parsley.Quapplicative` values.
-}
plugin :: Plugin
plugin = defaultPlugin { renamedResultAction = overloadedQuotes, pluginRecompile = purePlugin }

data QOps a = QOps {
    _code :: a,
    _val :: a,
    makeQ :: a,
    mkCode :: a,
    mkVal :: a
  } deriving (Functor, Foldable, Traversable, Generic)

quapplicativeStrings :: QOps String
quapplicativeStrings = QOps {
    _code = "_code",
    _val  = "_val",
    makeQ = "makeQ",
    mkCode = "mkCode",
    mkVal = "mkVal"
  }

overloadedQuotes :: [GHC.CommandLineOption] -> TcGblEnv -> GHC.HsGroup GHC.GhcRn -> TcM (TcGblEnv, GHC.HsGroup GHC.GhcRn)
overloadedQuotes _ gEnv rn = do
  hscEnv <- GHC.getTopEnv
  parsley <- lookupModule hscEnv "Parsley.Internal.Bridge"
  qops <- lookupNames parsley quapplicativeStrings
  -- This is a little inefficient, since the top-down transformation means no quotes can be
  -- found under a top-level one: we use a top-down version of everywhereBut to stop traversal
  -- of this whenever it fires
  return (gEnv, onlyTopmost (mkQ False isQuote) (mkT (transformUTHQuote qops)) rn)

mkApp :: GHC.SrcSpan -> Expr -> Expr -> Expr
mkApp s f = GHC.L s . Expr.HsApp noExt f . mkPar s

mkVar :: GHC.SrcSpan -> GHC.Name -> Expr
mkVar s = GHC.L s . Expr.HsVar noExt . GHC.L s

mkPar :: GHC.SrcSpan -> Expr -> Expr
mkPar s = GHC.L s . Expr.HsPar noExt

pattern LUTHQuote s ex1 ex2 x <- GHC.L s (Expr.HsRnBracketOut ex1 (Expr.ExpBr ex2 x) _)
pattern LUTHSplice s ex1 ex2 dec name x = GHC.L s (Expr.HsSpliceE ex1 (Expr.HsUntypedSplice ex2 dec name x))

isQuote :: Expr -> Bool
isQuote (LUTHQuote _ _ _ _) = True
isQuote _                   = False

-- The goal here is to find [|e|] and turn it into makeQ e [||e||]
-- The catch is that for any $qe in the quote, it must be hoisted out, let bound and then re-incorporated
-- such that `[|x .. $qe .. y|]` ~> `let qe' = qe in makeQ (x .. _val qe' .. y) [||x .. $$(_code qe') .. y||]`
-- This means that the content of the splice _cannot_ use any of the free-variables defined within
-- the original quote. Perhaps in that case we could just inline the definition into both holes...
-- As `transform` works bottom up, we can always assume nested quotes are already handled: this might
-- get tricky, however.
transformUTHQuote :: QOps GHC.Name -> Expr -> Expr
transformUTHQuote ops (LUTHQuote s ex ex' x) = --pprTouch "new quote" $
  mkPar s (makeQS `mkAppS` everywhere (mkT (transformUTHQuoteVar (_val ops) makeVal)) x
                  `mkAppS` mkQuote (everywhere (mkT (transformUTHQuoteCode (_code ops) makeCode)) x))
  where
    mkQuote y = GHC.L s (Expr.HsBracket ex (Expr.TExpBr ex' y))
    mkAppS = mkApp s
    makeQS = mkVar s (makeQ ops)
    mkValS = mkVar s (mkVal ops)
    mkCodeS = mkVar s (mkCode ops)
    makeVal y = mkPar s (mkValS `mkAppS` y)
    makeCode y = mkPar s (mkCodeS `mkAppS` y)
transformUTHQuote _ x = x

transformUTHQuoteVar :: GHC.Name -> (Expr -> Expr) -> Expr -> Expr
transformUTHQuoteVar _    makeVal (LUTHQuote _ _ _ x)      = makeVal x
transformUTHQuoteVar _val _       (LUTHSplice s _ _ _ _ x) = mkApp s (mkVar s _val) x
transformUTHQuoteVar _    _       x                        = x

transformUTHQuoteCode :: GHC.Name -> (Expr -> Expr) -> Expr -> Expr
transformUTHQuoteCode _     makeCode (LUTHQuote s ex ex' x)         = makeCode (GHC.L s (Expr.HsBracket ex (Expr.TExpBr ex' x)))
transformUTHQuoteCode _code _        (LUTHSplice s ex ex' d name x) = GHC.L s . Expr.HsSpliceE ex . Expr.HsTypedSplice ex' d name $
  mkApp s (mkVar s _code) x
transformUTHQuoteCode _     _        x                              = x

onlyTopmost :: GenericQ Bool -> GenericT -> GenericT
onlyTopmost q f = go
  where
    go :: GenericT
    go x
      | q x = f x
      | otherwise = gmapT go x