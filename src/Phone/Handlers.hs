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
import Phone.Internal.FFI.Common (CallId, PjIO)

data Handlers = Handlers
    { onCallStateChange :: Maybe (CallId -> PjIO ())
    , onIncomingCall :: Maybe (AccountId -> CallId -> PjIO ())
    , onRegistrationStateChange :: Maybe (AccountId -> PjIO ())
    , onRegistrationStarted :: Maybe (AccountId -> Int -> PjIO ())
    , onMediaStateChange :: Maybe (CallId -> PjIO ())
    }
