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
    ( OnCallStateHandler
    , OnIncomingCallHandler
    , OnMediaStateHandler
    , OnRegistrationStartedHandler
    , OnRegistrationStateHandler
    , PjSuaConfig
    , initializePjSua
    , setOnCallStateCallback
    , setOnIncomingCallCallback
    , setOnMediaStateCallback
    , setOnRegistrationStartedCallback
    , setOnRegistrationStateCallback
    , toOnCallState
    , toOnIncomingCall
    , toOnMediaState
    , toOnRegistrationStarted
    , toOnRegistrationState
    , withPjConfig
    )
  where

#include <pjsua-lib/pjsua.h>

import Data.Function (($))
import Foreign.C.Types (CInt(CInt))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable (pokeByteOff)
import System.IO (IO)

import Phone.Internal.FFI.Account (AccountId(AccountId))
import Phone.Internal.FFI.Common
    ( PjStatus(PjStatus)
    , CallId(CallId)
    , SipEvent
    , RxData
    )
import Phone.Internal.FFI.Logging (LoggingConfig)
import Phone.Internal.FFI.Media (MediaConfig)


data PjSuaConfig

type OnCallStateHandler = CallId -> Ptr SipEvent -> IO ()
type OnIncomingCallHandler = AccountId -> CallId -> Ptr RxData -> IO ()
type OnMediaStateHandler = CallId -> IO ()
type OnRegistrationStartedHandler = AccountId -> CInt -> IO ()
type OnRegistrationStateHandler = AccountId -> IO ()

withPjConfig :: (Ptr PjSuaConfig -> IO a) -> IO a
withPjConfig f = allocaBytes #{size pjsua_config} $ \cfg -> do
    defaultPjConfig cfg
    f cfg

foreign import ccall "pjsua_config_default" defaultPjConfig
    :: Ptr PjSuaConfig -> IO ()

-- | LoggingConfig and MediaConfig may be null
foreign import ccall "pjsua_init" initializePjSua
    :: Ptr PjSuaConfig -> Ptr LoggingConfig -> Ptr MediaConfig -> IO PjStatus

foreign import ccall safe "wrapper" toOnCallState
    :: OnCallStateHandler -> IO (FunPtr OnCallStateHandler)
foreign import ccall safe "wrapper" toOnIncomingCall
    :: OnIncomingCallHandler -> IO (FunPtr OnIncomingCallHandler)
foreign import ccall safe "wrapper" toOnMediaState
    :: OnMediaStateHandler -> IO (FunPtr OnMediaStateHandler)
foreign import ccall safe "wrapper" toOnRegistrationStarted
    :: OnRegistrationStartedHandler -> IO (FunPtr OnRegistrationStartedHandler)
foreign import ccall safe "wrapper" toOnRegistrationState
    :: OnRegistrationStateHandler -> IO (FunPtr OnRegistrationStateHandler)

setOnCallStateCallback :: Ptr PjSuaConfig -> FunPtr OnCallStateHandler -> IO ()
setOnCallStateCallback = #{poke pjsua_config, cb.on_call_state}

setOnIncomingCallCallback :: Ptr PjSuaConfig -> FunPtr OnIncomingCallHandler -> IO ()
setOnIncomingCallCallback = #{poke pjsua_config, cb.on_incoming_call}

setOnMediaStateCallback :: Ptr PjSuaConfig -> FunPtr OnMediaStateHandler -> IO ()
setOnMediaStateCallback = #{poke pjsua_config, cb.on_call_media_state}

setOnRegistrationStartedCallback :: Ptr PjSuaConfig -> FunPtr OnRegistrationStartedHandler -> IO ()
setOnRegistrationStartedCallback = #{poke pjsua_config, cb.on_reg_started}

setOnRegistrationStateCallback :: Ptr PjSuaConfig -> FunPtr OnRegistrationStateHandler -> IO ()
setOnRegistrationStateCallback = #{poke pjsua_config, cb.on_reg_state}
