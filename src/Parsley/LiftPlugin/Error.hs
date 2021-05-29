{-# LANGUAGE CPP #-}
module Parsley.LiftPlugin.Error (addErrTc, getErrTc, makeError) where

-- GHC API
import TcRnTypes
import Outputable
-- ghc
import qualified GHC hiding (exprType)
#if __GLASGOW_HASKELL__ < 810
import qualified HsExpr as Expr
#else
import qualified GHC.Hs.Expr as Expr
#endif

import qualified TcRnMonad as GHC

import Control.Monad.IO.Class ( liftIO )
import Control.Monad
import Data.IORef
import System.IO.Unsafe

-- In this IORef we store how we would have reported the error
ioRef :: IORef (Int, [TcM ()])
ioRef = unsafePerformIO (newIORef (0, []))
{-# NOINLINE ioRef #-}

addError :: TcM () -> IO Int
addError err = atomicModifyIORef ioRef (\(k, errs) -> ((k+1, errs ++ [err]), k))

getError :: Int -> IO (TcM ())
getError k = (!! k) . snd <$> readIORef ioRef

addErrTc :: TcM () -> TcPluginM Int
addErrTc err = unsafeTcPluginTcM (liftIO (addError err))

getErrTc :: Int -> TcM ()
getErrTc k = join (liftIO (getError k))

makeError :: Expr.LHsExpr GHC.GhcTc -> GHC.TcM ()
makeError (GHC.L l e) =
  GHC.setSrcSpan l $
  GHC.addErrCtxt (text "In" <+> ppr e) $
  GHC.failWithTc msg
  where
    msg = text "Unable to magically lift the argument."
          <+> text "It probably isn't statically known."