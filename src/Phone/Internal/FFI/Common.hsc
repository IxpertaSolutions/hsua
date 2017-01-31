{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
module Phone.Internal.FFI.Common
    ( CallId(CallId)
    , CallSetting
    , PjIO(PjIO)
    , PjStatus(PjStatus)
    , Reason
    , UserData
    , Transaction
    , liftAlloc
    , pjFalse
    , pjSuccess
    , pjTrue
    , runPjIO
    )
  where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import Data.Eq (Eq)
import Data.Ord (Ord)
import Data.Function ((.))
import Data.Functor (Functor)
import Foreign.C.Types (CInt)
import Foreign.Storable (Storable)
import System.IO (IO)
import Text.Show (Show)

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Phone.Internal.Thread (execInBoundWorker)

#include <pjsua-lib/pjsua.h>


newtype PjIO a = PjIO { unsafeRunPjIO :: IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

runPjIO :: PjIO a -> IO a
runPjIO = execInBoundWorker . unsafeRunPjIO

liftAlloc :: ((a -> IO b) -> IO b) -> ((a -> PjIO b) -> PjIO b)
liftAlloc f = liftIO . f . (unsafeRunPjIO .)

data CallSetting
data Reason
data UserData
data Transaction

newtype CallId = CallId CInt deriving (Eq, Ord, Show, Storable)
newtype PjStatus = PjStatus CInt deriving (Eq, Show, Storable)

-- | Equivalent to PJ_TRUE
pjTrue :: CInt
pjTrue = #{const PJ_TRUE}

-- | Equivalent to PJ_FALSE
pjFalse :: CInt
pjFalse = #{const PJ_FALSE}

#{enum PjStatus, PjStatus, pjSuccess = PJ_SUCCESS}
