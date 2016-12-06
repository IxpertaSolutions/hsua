{-# LANGUAGE ForeignFunctionInterface #-}
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
module Phone.Internal.FFI.CallInfo
    ( CallState(..)
    , getAccountId
    , getCallInfo
    , getCallState
    , withCallInfo
    )
  where

#include <pjsua-lib/pjsua.h>

import Data.Eq (Eq)
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import Foreign.C.Types (CInt(CInt))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import Prelude (Enum, toEnum, fromEnum, fromIntegral, error)
import System.IO (IO)
import Text.Show (Show, show)

import Phone.Internal.FFI.Account (AccountId)
import Phone.Internal.FFI.Common
    ( CallId
    , PjStatus
    )


data CallInfo

withCallInfo :: (Ptr CallInfo -> IO a) -> IO a
withCallInfo = allocaBytes #{size pjsua_call_info}

foreign import ccall "pjsua_call_get_info" getCallInfo
    :: CallId
    -> Ptr CallInfo
    -> IO PjStatus

getAccountId :: Ptr CallInfo -> IO AccountId
getAccountId = #{peek pjsua_call_info, acc_id}

data CallState
    = Null -- ^ Before INVITE is sent or received
    | Calling -- ^ After INVITE is sent
    | Incoming -- ^ After INVITE is received.
    | EarlyMedia -- ^ After response with To tag.
    | Connecting -- ^ After 2xx is sent/received.
    | Confirmed -- ^ After ACK is sent/received.
    | Disconnected -- ^ Session is terminated.
  deriving (Eq,Show)

instance Enum CallState where
    fromEnum Null = #{const PJSIP_INV_STATE_NULL}
    fromEnum Calling = #{const PJSIP_INV_STATE_CALLING}
    fromEnum Incoming = #{const PJSIP_INV_STATE_INCOMING}
    fromEnum EarlyMedia = #{const PJSIP_INV_STATE_EARLY}
    fromEnum Connecting = #{const PJSIP_INV_STATE_CONNECTING}
    fromEnum Confirmed = #{const PJSIP_INV_STATE_CONFIRMED}
    fromEnum Disconnected = #{const PJSIP_INV_STATE_DISCONNECTED}

    toEnum #{const PJSIP_INV_STATE_NULL} = Null
    toEnum #{const PJSIP_INV_STATE_CALLING} = Calling
    toEnum #{const PJSIP_INV_STATE_INCOMING} = Incoming
    toEnum #{const PJSIP_INV_STATE_EARLY} = EarlyMedia
    toEnum #{const PJSIP_INV_STATE_CONNECTING} = Connecting
    toEnum #{const PJSIP_INV_STATE_CONFIRMED} = Confirmed
    toEnum #{const PJSIP_INV_STATE_DISCONNECTED} = Disconnected
    toEnum unmatched =
        error ("CallState toEnum error with: " <> show unmatched)

getCallState :: Ptr CallInfo -> IO CallState
getCallState callInfo = (toEnum . fromIntegral) <$> c_getCallState callInfo

c_getCallState :: Ptr CallInfo -> IO CInt
c_getCallState = #{peek pjsua_call_info, state}
