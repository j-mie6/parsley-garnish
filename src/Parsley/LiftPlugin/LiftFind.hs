{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
module Parsley.LiftPlugin.LiftFind (findLiftPlugin) where

-- external
import Data.Maybe          (mapMaybe)
import qualified GHC.TcPluginM.Extra as TCPluginExtra (lookupModule)

-- GHC API
import FastString (fsLit)
import Module     (mkModuleName)
import TcEvidence
import TcPluginM  (tcLookupTyCon)
import TcRnTypes
#if __GLASGOW_HASKELL__ >= 810
import Constraint
#endif
import TyCon      (TyCon)
import Class
import Bag
import TcErrors
-- ghc
import qualified GhcPlugins as GHC
#if __GLASGOW_HASKELL__ >= 810
import qualified Predicate as GHC
#endif

import Parsley.PluginUtils (lookupClass)
import Parsley.LiftPlugin.Error (addErrTc)
import Parsley.LiftPlugin.Fake (pattern FakeExpr)

findLiftPlugin :: TcPlugin
findLiftPlugin =
  TcPlugin { tcPluginInit  = lookupLiftTyCon
           , tcPluginSolve = replaceLiftWithDummy
           , tcPluginStop  = const (return ())
           }

-- TODO: This is going to need to be augmented with unsafeTExpCoerce/unsafeCodeCoerce
type LiftEnv = TyCon
lookupLiftTyCon :: TcPluginM LiftEnv
lookupLiftTyCon = do
    md       <- TCPluginExtra.lookupModule liftModule liftPackage
    liftTcNm <- lookupClass md "Lift"
    tcLookupTyCon liftTcNm
  where
    liftModule  = mkModuleName "Language.Haskell.TH.Syntax"
    liftPackage = fsLit "template-haskell"

-- This plugin solves all instances of (Lift (a -> b)) with a dummy value.
replaceLiftWithDummy :: LiftEnv -- ^ Lift's TyCon
                     -> [Ct]    -- ^ [G]iven constraints
                     -> [Ct]    -- ^ [D]erived constraints
                     -> [Ct]    -- ^ [W]anted constraints
                     -> TcPluginM TcPluginResult
replaceLiftWithDummy _      _   _   []      = return (TcPluginOk [] [])
replaceLiftWithDummy liftTc _gs _ds wanteds = fmap (flip TcPluginOk []) (mapM (\c -> (, c) <$> evMagic c) (mapMaybe toLiftCt wanteds))
  where
    toLiftCt :: Ct -> Maybe Ct
    toLiftCt ct =
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

-- | TODO: Check here for (->) instance
evMagic :: Ct -> TcPluginM EvTerm
evMagic ct = do
  k <- addErrTc (reportAllUnsolved (WC (unitBag ct) emptyBag))
  return $ EvExpr (FakeExpr (ctPred ct) k)