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
module Phone.Internal.FFI.Configuration
  where

import Foreign.C.Types (CInt(CInt))
import Foreign.Ptr (FunPtr, Ptr)

import System.IO (IO)

import Phone.Internal.FFI.Common (PjStatus, CallId, SipEvent, RxData)
import Phone.Internal.FFI.Logging (LoggingConfig)
import Phone.Internal.FFI.Media (MediaConfig)
import Phone.Internal.FFI.Account (AccountId)


data PjSuaConfig

foreign import ccall "create_pj_config" createPjConfig :: IO (Ptr PjSuaConfig)
foreign import ccall "pjsua_config_default" defaultPjConfig
    :: Ptr PjSuaConfig -> IO ()

-- | Looks like the LoggingConfig and MediaConfig may be null
foreign import ccall "pjsua_init" initializePjSua
    :: Ptr PjSuaConfig -> Ptr LoggingConfig -> Ptr MediaConfig -> IO PjStatus

type OnCallStateHandler = CallId -> Ptr SipEvent -> IO ()

foreign import ccall safe "wrapper"
    toOnCallState :: OnCallStateHandler -> IO (FunPtr OnCallStateHandler)

foreign import ccall "pjsua_set_on_call_state" setOnCallStateCallback
    :: Ptr PjSuaConfig -> FunPtr OnCallStateHandler -> IO ()

type OnIncomingCallHandler = AccountId -> CallId -> Ptr RxData -> IO ()

foreign import ccall safe "wrapper" toOnIncomingCall
    :: OnIncomingCallHandler -> IO (FunPtr OnIncomingCallHandler)

foreign import ccall "pjsua_set_on_incoming_call" setOnIncomingCallCallback
    :: Ptr PjSuaConfig -> FunPtr OnIncomingCallHandler -> IO ()

type OnRegistrationStateHandler = AccountId -> IO ()

foreign import ccall safe "wrapper" toOnRegistrationState
    :: OnRegistrationStateHandler -> IO (FunPtr OnRegistrationStateHandler)

foreign import ccall "pjsua_set_on_reg_state" setOnRegistrationStateCallback
    :: Ptr PjSuaConfig -> FunPtr OnRegistrationStateHandler -> IO ()

type OnMediaStateHandler = CallId -> IO ()

foreign import ccall safe "wrapper" toOnMediaState
    :: OnMediaStateHandler -> IO (FunPtr OnMediaStateHandler)

foreign import ccall "pjsua_set_on_media_state" setOnMediaStateCallback
    :: Ptr PjSuaConfig -> FunPtr OnMediaStateHandler -> IO ()
