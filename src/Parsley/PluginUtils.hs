{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Parsley.PluginUtils where

import qualified GHC.TcPluginM.Extra as TCPluginExtra (lookupName)

-- GHC API
import TcRnTypes (TcM, TcPluginM)
import Outputable

-- ghc
import qualified GhcPlugins as GHC
import qualified IfaceEnv as GHC (lookupOrig)
import Finder (findImportedModule, FindResult(Found))
import FastString (mkFastString)
import Module (Module, mkModuleName)
import Name (Name)
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
  Found _ md <- liftIO (findImportedModule hscEnv (mkModuleName ("Parsley." ++ modName)) Nothing)
  return md

lookupModuleInPackage :: GHC.HscEnv -> String -> String -> TcM Module
lookupModuleInPackage hscEnv package modName = do
  Found _ md <- liftIO (findImportedModule hscEnv (mkModuleName modName) (Just (mkFastString package)))
  return md