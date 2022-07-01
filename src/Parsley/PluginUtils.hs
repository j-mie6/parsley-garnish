{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, CPP #-}
module Parsley.PluginUtils where

import qualified GHC.TcPluginM.Extra as TCPluginExtra (lookupName)

-- ghc
#if __GLASGOW_HASKELL__ >= 900
import GHC.Tc.Types (TcM, TcPluginM)
import GHC.Utils.Outputable
import qualified GHC.Plugins as GHC
import qualified GHC.Iface.Env as GHC (lookupOrig)
#if __GLASGOW_HASKELL__ == 900
import GHC.Driver.Finder (findImportedModule, FindResult(Found))
#else
import GHC.Unit.Finder (findImportedModule, FindResult(Found))
#endif
import GHC.Data.FastString (mkFastString)
import GHC.Unit.Module (Module)
import GHC.Unit.Module.Name (mkModuleName)
import GHC (Name)
#else
import TcRnTypes (TcM, TcPluginM)
import Outputable
import qualified GhcPlugins as GHC
import qualified IfaceEnv as GHC (lookupOrig)
import Finder (findImportedModule, FindResult(Found))
import FastString (mkFastString)
import Module (Module, mkModuleName)
import Name (Name)
#endif

#if __GLASGOW_HASKELL__ > 900
import qualified GHC.Types.TyThing as GHC
import GHC.Plugins (pprTrace)
#endif

import Control.Monad.IO.Class ( liftIO )

class Monad m => Lookup m where
  lookupOrig :: Module -> GHC.OccName -> m Name

instance Lookup TcM where
  lookupOrig = GHC.lookupOrig

instance Lookup TcPluginM where
  lookupOrig = TCPluginExtra.lookupName

pprTouch :: Outputable a => String -> a -> a
pprTouch name x = pprTrace name (ppr x) x

lookupNames :: (Lookup m, Traversable t) => Module -> t String -> m (t Name)
lookupNames = traverse . lookupName

lookupName :: Lookup m => Module -> String -> m Name
lookupName pm = lookupOrig pm . GHC.mkVarOcc

lookupClass :: Lookup m => Module -> String -> m Name
lookupClass pm = lookupOrig pm . GHC.mkTcOcc

lookupIds :: Traversable t => Module -> t String -> TcM (t GHC.Id)
lookupIds = traverse . lookupId

lookupId :: Module -> String -> TcM GHC.Id
lookupId pm name = lookupName pm name >>= GHC.lookupId

lookupModule :: GHC.HscEnv -> String -> TcM Module
lookupModule hscEnv modName = do
  Found _ md <- liftIO (findImportedModule hscEnv (mkModuleName modName) Nothing)
  return md

lookupModuleInPackage :: GHC.HscEnv -> String -> String -> TcM Module
lookupModuleInPackage hscEnv package modName = do
  Found _ md <- liftIO (findImportedModule hscEnv (mkModuleName modName) (Just (mkFastString package)))
  return md
