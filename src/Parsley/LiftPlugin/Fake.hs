{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
module Parsley.LiftPlugin.Fake (pattern FakeExpr, getFakeDicts) where

-- external
import Data.Maybe (mapMaybe)

-- GHC API
import FastString (fsLit)
import TcEvidence
import TyCoRep    (Type (..))
import MkCore
import CoreSyn
import Literal
import PrelNames
import qualified Unique as GHC
-- ghc
import qualified GHC
import qualified GhcPlugins as GHC
#if __GLASGOW_HASKELL__ >= 810
import qualified TyCoRep as GHC
#endif

import qualified TysPrim as GHC

-- What this is doens't matter really
fakeIdName :: GHC.Name
fakeIdName = GHC.mkWiredInIdName gHC_MAGIC (fsLit "fake_id") fakeIdKey fakeId

fakeIdKey :: Unique
fakeIdKey = GHC.mkPreludeMiscIdUnique 109

fakeId :: GHC.Id
fakeId = GHC.mkVanillaGlobalWithInfo fakeIdName ty GHC.noCafIdInfo
  where
#if __GLASGOW_HASKELL__ < 810
    ty = GHC.mkSpecForAllTys [GHC.alphaTyVar] (GHC.mkFunTy GHC.intTy GHC.alphaTy)
#else
    ty = GHC.mkSpecForAllTys [GHC.alphaTyVar] (GHC.mkFunTy GHC.VisArg GHC.intTy GHC.alphaTy)
#endif

pattern FakeExpr :: Type -> Int -> Expr GHC.Id
pattern FakeExpr ty k <- App (App (Var ((== fakeId)-> True)) (Type ty)) (Lit (LitNumber LitNumInteger (fromIntegral -> k) _))
  where
    FakeExpr ty k = mkCoreApps (Var fakeId) [Type ty, (Lit $ LitNumber LitNumInteger (fromIntegral k) GHC.intTy)]

getFakeDicts :: [EvBind] -> [(GHC.EvVar, Int)]
getFakeDicts = mapMaybe getFakeDict
  where
    getFakeDict (EvBind r (EvExpr (FakeExpr _ k)) _) = Just (r, k)
    getFakeDict _                                    = Nothing