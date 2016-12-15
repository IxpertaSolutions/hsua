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
module Phone.Internal.FFI.Event
    ( Event
    , EventType(..)
    , getEventType
    , getRxData
    )
  where

#include <pjsua-lib/pjsua.h>

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Monoid ((<>))
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekByteOff)
import Prelude (Enum, toEnum, fromEnum, fromIntegral, error)
import Text.Show (Show, show)

import Phone.Internal.FFI.RxData (RxData)
import Phone.Internal.FFI.Common (PjIO)


data Event

getEventType :: Ptr Event -> PjIO EventType
getEventType ptr = (toEnum . fromIntegral) <$> getEventType' ptr

getEventType' :: Ptr Event -> PjIO CInt
getEventType' = liftIO . #{peek pjsip_event, type}

data EventType =
    Unknown
    | Timer
    | TxMsg
    | RxMsg
    | TransportError
    | TransactionState
    | User
  deriving (Eq, Show)

instance Enum EventType where
    toEnum #{const PJSIP_EVENT_UNKNOWN} = Unknown
    toEnum #{const PJSIP_EVENT_TIMER} = Timer
    toEnum #{const PJSIP_EVENT_TX_MSG} = TxMsg
    toEnum #{const PJSIP_EVENT_RX_MSG} = RxMsg
    toEnum #{const PJSIP_EVENT_TRANSPORT_ERROR} = TransportError
    toEnum #{const PJSIP_EVENT_TSX_STATE} = TransactionState
    toEnum #{const PJSIP_EVENT_USER} = User
    toEnum unmatched =
        error ("CallState toEnum error with: " <> show unmatched)

    fromEnum Unknown = #{const PJSIP_EVENT_UNKNOWN}
    fromEnum Timer = #{const PJSIP_EVENT_TIMER}
    fromEnum TxMsg = #{const PJSIP_EVENT_TX_MSG}
    fromEnum RxMsg = #{const PJSIP_EVENT_RX_MSG}
    fromEnum TransportError = #{const PJSIP_EVENT_TRANSPORT_ERROR}
    fromEnum TransactionState = #{const PJSIP_EVENT_TSX_STATE}
    fromEnum User = #{const PJSIP_EVENT_USER}

getRxData :: Ptr Event -> Ptr RxData
getRxData = #{ptr pjsip_event, body.rx_msg.rdata}

