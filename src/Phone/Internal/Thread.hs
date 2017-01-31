{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Low level FFI.
-- Copyright:
-- License:      GPL-2
--
-- Maintainer:   Jan Sipr <jan.sipr@ixperta.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
module Phone.Internal.Thread
    ( execInBoundWorker
    )
  where

import Control.Applicative (pure)
import Control.Concurrent (forkOS)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, mask, throwIO, try)
import Control.Monad ((>>=), forever, join, void)
import Data.Either (either)
import Data.Function (($), (.))
import System.IO (IO)
import System.IO.Unsafe (unsafePerformIO)


{-# NOINLINE queue #-}
queue :: MVar (IO ())
queue = unsafePerformIO $ do
    mvar <- newEmptyMVar
    void . forkOS $ worker mvar
    pure mvar

worker :: MVar (IO ()) -> IO ()
worker = forever . join . takeMVar

execInBoundWorker :: IO a -> IO a
execInBoundWorker action = do
    result <- newEmptyMVar
    putMVar queue $ mask $ \restore -> try (restore action) >>= putMVar result
    takeMVar result >>= either (throwIO :: SomeException -> IO a) pure
