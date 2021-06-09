{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
module Parsley.OverloadedQuotesPlugin.Plugin where

import Plugins       (Plugin (..), defaultPlugin, purePlugin)
import TcRnTypes     (TcGblEnv, TcM)
import Data.Generics (mkT, everywhere, everywhere')
import GHC.Generics  (Generic)

import qualified GHC               (HsGroup, GhcRn, Name, GenLocated(L), SrcSpan)
import qualified GhcPlugins as GHC (CommandLineOption)
import qualified TcRnMonad as GHC  (getTopEnv)

#if __GLASGOW_HASKELL__ < 810
import qualified HsExpr as Expr
#else
import qualified GHC.Hs.Expr as Expr
#endif

import Parsley.PluginUtils (lookupModuleInPackage, lookupModule, lookupName, lookupNames, pprTouch)

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
  parsley <- lookupModuleInPackage hscEnv "parsley" "Parsley.Internal.Common.Utils"
  qops <- lookupNames parsley quapplicativeStrings
  prelude <- lookupModuleInPackage hscEnv "base" "GHC.Err"
  undef <- lookupName prelude "undefined"
  pprTouch "names" [makeQ qops, _code qops, _val qops] `seq` return ()
  return (gEnv, everywhere' (mkT (transformUTHQuote qops undef)) rn)

mkApp :: GHC.SrcSpan -> Expr -> Expr -> Expr
mkApp s f = GHC.L s . Expr.HsApp noExt f . mkPar s

mkVar :: GHC.SrcSpan -> GHC.Name -> Expr
mkVar s = GHC.L s . Expr.HsVar noExt . GHC.L s

mkPar :: GHC.SrcSpan -> Expr -> Expr
mkPar s = GHC.L s . Expr.HsPar noExt

-- The goal here is to find [|e|] and turn it into makeQ e [||e||]
-- The catch is that for any $qe in the quote, it must be hoisted out, let bound and then re-incorporated
-- such that `[|x .. $qe .. y|]` ~> `let qe' = qe in makeQ (x .. _val qe' .. y) [||x .. $$(_code qe') .. y||]`
-- This means that the content of the splice _cannot_ use any of the free-variables defined within
-- the original quote. Perhaps in that case we could just inline the definition into both holes...
-- As `transform` works bottom up, we can always assume nested quotes are already handled: this might
-- get tricky, however.
transformUTHQuote :: QOps GHC.Name -> GHC.Name -> Expr -> Expr
transformUTHQuote ops undef (GHC.L s (Expr.HsRnBracketOut ex (Expr.ExpBr ex' x) _)) = pprTouch "new quote" $ 
  mkPar s (makeQS `mkAppS` everywhere (mkT (transformUTHQuoteVar (_val ops) makeVal)) x 
                  `mkAppS` mkQuote (everywhere (mkT (transformUTHQuoteCode (_code ops) makeCode)) x))
  where
    mkQuote y = GHC.L s (Expr.HsBracket ex (Expr.TExpBr ex' y))
    mkAppS = mkApp s
    makeQS = mkVar s (makeQ ops)
    makeVal y = mkPar s (makeQS `mkAppS` y `mkAppS` mkVar s undef)
    makeCode y = mkPar s (makeQS `mkAppS` mkVar s undef `mkAppS` y)

  --GHC.L s (Expr.HsBracket ex (Expr.TExpBr ex' x))
--transformUTHQuote _ _ (GHC.L s (Expr.HsSpliceE ex (Expr.HsUntypedSplice ex' d x y))) =  pprTouch "new splice" $ GHC.L s (Expr.HsSpliceE ex (Expr.HsTypedSplice ex' d x y))
transformUTHQuote _ _ x = x

transformUTHQuoteVar :: GHC.Name -> (Expr -> Expr) -> Expr -> Expr
transformUTHQuoteVar _    makeVal (GHC.L _ (Expr.HsRnBracketOut _ (Expr.ExpBr _ x) _))        = makeVal x
transformUTHQuoteVar _val _       (GHC.L s (Expr.HsSpliceE _ (Expr.HsUntypedSplice _ _ _ x))) = mkApp s (mkVar s _val) x
transformUTHQuoteVar _    _       x                                                           = x

transformUTHQuoteCode :: GHC.Name -> (Expr -> Expr) -> Expr -> Expr
transformUTHQuoteCode _     makeCode (GHC.L s (Expr.HsRnBracketOut ex (Expr.ExpBr ex' x) _))           = makeCode (GHC.L s (Expr.HsBracket ex (Expr.TExpBr ex' x)))
transformUTHQuoteCode _code _        (GHC.L s (Expr.HsSpliceE ex (Expr.HsUntypedSplice ex' d name x))) = GHC.L s . Expr.HsSpliceE ex . Expr.HsTypedSplice ex' d name $
  mkApp s (mkVar s _code) x
transformUTHQuoteCode _     _        x                                                                 = x

{-

transformUQuote :: Exp -> Exp
transformUQuote q@(UQu x) = trace ("quote to process: " ++ show q ++ "\nlocal bindings: " ++ show (findBindings x)) $
  makeQ (everywhere (mkT transformUQuoteVar) x) (TQu (everywhere (mkT transformUQuoteCode) x))
transformUQuote x         = x

-- So far we have the top level quote, along with the bindings found within
-- we need to process the quotes inside. In fact, we should take the expression and
-- process it twice, once assuming _code and the other assuming _var. Maybe then the
-- bindings aren't important as everything is treated equally?

-- It will be important to rename the captured bindings when the expression is duplicated

transformUQuoteVar :: Exp -> Exp
transformUQuoteVar (UQu x) = makeVal x
transformUQuoteVar (USp x) = _val x
transformUQuoteVar x       = x

transformUQuoteCode :: Exp -> Exp
transformUQuoteCode (UQu x) = makeCode x
transformUQuoteCode (USp x) = TSp (_code x)
transformUQuoteCode x       = x


-}