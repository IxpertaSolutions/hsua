{-# LANGUAGE DeriveGeneric #-}
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
module Phone.Config
    ( Config(..)
    , Handlers(..)
    , Logging(..)
    )
  where

import Data.Bool (Bool)
import Data.Int (Int)
import Data.Maybe (Maybe)
import Data.String (String)
import Data.Word (Word)
import GHC.Generics (Generic)
import Text.Show (Show)

import Data.Default (Default)

import Phone.Internal.Event (Event)
import Phone.Internal.FFI.Account (AccountId)
import Phone.Internal.FFI.Common (CallId, PjIO)


data Config = Config
    { handlers :: Handlers
    , logging :: Logging
    }
  deriving (Generic)

instance Default Config

data Handlers = Handlers
    { onCallStateChange :: Maybe (CallId -> Event -> PjIO ())
    , onCallTransactionStateChange :: Maybe (CallId -> Event -> PjIO ())
    , onIncomingCall :: Maybe (AccountId -> CallId -> PjIO ())
    , onMediaStateChange :: Maybe (CallId -> PjIO ())
    , onRegistrationStarted :: Maybe (AccountId -> Int -> PjIO ())
    , onRegistrationStateChange :: Maybe (AccountId -> PjIO ())
    }
  deriving (Generic)

instance Default Handlers

data Logging = Logging
    { logLevel :: Maybe Word
    , logConsoleLevel :: Maybe Word
    , logMsgLogging :: Maybe Bool
    , logFilename :: Maybe String
    }
    deriving (Show, Generic)

instance Default Logging
