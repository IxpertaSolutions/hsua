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
module Phone.Internal.FFI.CallManipulation
  where

#include <pjsua-lib/pjsua.h>

import Foreign.C.Types (CInt(CInt), CUInt(CUInt))
import Foreign.Ptr (Ptr)

import System.IO (IO)

import Phone.Internal.FFI.Common
    ( CallId
    , CallSetting
    , MsgData
    , PjStatus
    , PjString
    , Reason
    , UserData
    )
import Phone.Internal.FFI.Account (AccountId)

-- | Send response to incoming INVITE request with call setting param.
-- Depending on the status code specified as parameter, this function may send
-- provisional response, establish the call, or terminate the call. Notes about
-- call setting:
--
--  * if call setting is changed in the subsequent call to this function, only
--    the first call setting supplied will applied. So normally application
--    will not supply call setting before getting confirmation from the user.
--  * if no call setting is supplied when SDP has to be sent, i.e: answer with
--    status code 183 or 2xx, the default call setting will be used, check
--    pjsua_call_setting for its default values.
foreign import ccall "pjsua_call_answer" answerCall
    :: CallId
    -> CUInt
    -- ^ Status code to be used to answer the call.
    -> Ptr Reason
    -- ^ Optional reason phrase which will be find into SIP header. If null,
    -- the default phrase will be used.
    -> Ptr MsgData
    -- ^ Optional list of headers to be added to SIP msg.
    -> IO PjStatus

-- | Hangup call by using method that is appropriate according to the call
-- state. This function is different than answering the call with 3xx-6xx
-- response (with pjsua_call_answer()), in that this function will hangup the
-- call regardless of the state and role of the call, while pjsua_call_answer()
-- only works with incoming calls on EARLY state.
foreign import ccall "pjsua_call_hangup" hangupCall
    :: CallId
    -> CUInt
    -- ^ Status code to be used to hangup the call.
    -> Ptr Reason
    -- ^ Optional reason phrase which will be find into SIP
    --   header. If null, the default phrase will be used.
    -> Ptr MsgData
    -- ^ Optional list of headers to be added to SIP msg.
    -> IO PjStatus

-- | Make call to specified URI.
foreign import ccall "pjsua_call_make_call" makeCall
    :: AccountId
    -- ^ Account id for account we want to originate the call.
    -> Ptr PjString
    -- ^ Destination URI in format \"sip:(name/number)@address.com\".
    -> Ptr CallSetting
    -- ^ Optional ('nullPtr') call settings.
    -> Ptr UserData
    -- ^ Optional ('nullPtr') arbitrary user data to be attached to the call,
    -- and can be retrieved later.
    -> Ptr MsgData
    -- ^ Optional ('nullPtr') headers to be added to SIP msg.
    -> Ptr CallId
    -- ^ Optional ('nullPtr') Pointer to CallId where the call id will be
    -- stored.
    -> IO PjStatus

-- | Terminate (end) all calls. In other words this will call 'callHangup' to
-- all currently active calls.
foreign import ccall "pjsua_call_hangup_all" hangupAll :: IO ()
