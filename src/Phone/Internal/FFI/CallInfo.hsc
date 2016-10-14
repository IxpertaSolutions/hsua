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
  where

-- GHC lower than 8.0 don't have alignment macro.
#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

-- This allows to retrieve value from enums and defines
-- Pjsua uses extremely tricky enums...
#let enumToValue t = "%d", (int)t

#include <pjsua-lib/pjsua.h>

import Data.Eq (Eq)
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import Foreign.C.Types (CInt(CInt))
import Foreign.Ptr (Ptr)
import Prelude (Enum, toEnum, fromEnum, fromIntegral, error)
import System.IO (IO)
import Text.Show (Show, show)

import Phone.Internal.FFI.Account (AccountId)
import Phone.Internal.FFI.Common
    ( CallId
    , PjStatus
    )


data CallInfo

foreign import ccall "create_pjsua_call_info" createCallInfo
    :: IO (Ptr CallInfo)

foreign import ccall "pjsua_call_get_info" getCallInfo
    :: CallId
    -> Ptr CallInfo
    -> IO PjStatus

foreign import ccall "getAccountId" getAccountId
    :: Ptr CallInfo
    -> IO AccountId

data CallState =
    Null -- ^ Before INVITE is sent or received
    | Calling -- ^ After INVITE is sent
    | Incoming -- ^ After INVITE is received.
    | EarlyMedia -- ^ After response with To tag.
    | Connecting -- ^ After 2xx is sent/received.
    | Confirmed -- ^ After ACK is sent/received.
    | Disconnected -- ^ Session is terminated.
  deriving (Eq,Show)

instance Enum CallState where
    fromEnum Null = #{enumToValue PJSIP_INV_STATE_NULL}
    fromEnum Calling = #{enumToValue PJSIP_INV_STATE_CALLING}
    fromEnum Incoming = #{enumToValue PJSIP_INV_STATE_INCOMING}
    fromEnum EarlyMedia = #{enumToValue PJSIP_INV_STATE_EARLY}
    fromEnum Connecting = #{enumToValue PJSIP_INV_STATE_CONNECTING}
    fromEnum Confirmed = #{enumToValue PJSIP_INV_STATE_CONFIRMED}
    fromEnum Disconnected = #{enumToValue PJSIP_INV_STATE_DISCONNECTED}

    toEnum #{enumToValue PJSIP_INV_STATE_NULL} = Null
    toEnum #{enumToValue PJSIP_INV_STATE_CALLING} = Calling
    toEnum #{enumToValue PJSIP_INV_STATE_INCOMING} = Incoming
    toEnum #{enumToValue PJSIP_INV_STATE_EARLY} = EarlyMedia
    toEnum #{enumToValue PJSIP_INV_STATE_CONNECTING} = Connecting
    toEnum #{enumToValue PJSIP_INV_STATE_CONFIRMED} = Confirmed
    toEnum #{enumToValue PJSIP_INV_STATE_DISCONNECTED} = Disconnected
    toEnum unmatched =
        error ("CallState toEnum error with: " <> show unmatched)

getCallState :: Ptr CallInfo -> IO CallState
getCallState callInfo = (toEnum . fromIntegral) <$> c_getCallState callInfo

foreign import ccall "getCallState" c_getCallState
    :: Ptr CallInfo
    -> IO CInt

