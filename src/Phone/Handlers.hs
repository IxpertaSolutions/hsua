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
module Phone.Handlers
    ( Handlers(..)
    )
  where

import Data.Int (Int)
import Data.Maybe (Maybe)
import Phone.Internal.FFI.Account (AccountId)
import Phone.Internal.FFI.Common (CallId)
import System.IO (IO)

data Handlers = Handlers
    { onCallStateChange :: Maybe (CallId -> IO ())
    , onIncomingCall :: Maybe (AccountId -> CallId -> IO ())
    , onRegistrationStateChange :: Maybe (AccountId -> IO ())
    , onRegistrationStarted :: Maybe (AccountId -> Int -> IO ())
    , onMediaStateChange :: Maybe (CallId -> IO ())
    }
