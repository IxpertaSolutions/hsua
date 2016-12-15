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

import Data.Function (($), (.))
import Foreign.C.Types (CInt(CInt))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable (pokeByteOff)
import System.IO (IO)

import Control.Monad.IO.Class (liftIO)

import Phone.Internal.FFI.Account (AccountId(AccountId))
import Phone.Internal.FFI.Common
    ( PjIO(PjIO)
    , PjStatus(PjStatus)
    , CallId(CallId)
    , liftAlloc
    )
import Phone.Internal.FFI.Logging (LoggingConfig)
import Phone.Internal.FFI.Media (MediaConfig)
import Phone.Internal.FFI.RxData (RxData)
import Phone.Internal.FFI.Event (Event)


data PjSuaConfig

type OnCallStateHandler = CallId -> Ptr Event -> PjIO ()
type OnIncomingCallHandler = AccountId -> CallId -> Ptr RxData -> PjIO ()
type OnMediaStateHandler = CallId -> PjIO ()
type OnRegistrationStartedHandler = AccountId -> CInt -> PjIO ()
type OnRegistrationStateHandler = AccountId -> PjIO ()

withPjConfig :: (Ptr PjSuaConfig -> PjIO a) -> PjIO a
withPjConfig f = liftAlloc (allocaBytes #{size pjsua_config}) $ \cfg -> do
    defaultPjConfig cfg
    f cfg

foreign import ccall "pjsua_config_default" defaultPjConfig
    :: Ptr PjSuaConfig -> PjIO ()

-- | LoggingConfig and MediaConfig may be null
foreign import ccall "pjsua_init" initializePjSua
    :: Ptr PjSuaConfig -> Ptr LoggingConfig -> Ptr MediaConfig -> PjIO PjStatus

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

setOnCallStateCallback
    :: Ptr PjSuaConfig -> FunPtr OnCallStateHandler -> PjIO ()
setOnCallStateCallback =
    (liftIO .) . #{poke pjsua_config, cb.on_call_state}

setOnIncomingCallCallback
    :: Ptr PjSuaConfig -> FunPtr OnIncomingCallHandler -> PjIO ()
setOnIncomingCallCallback =
    (liftIO .) . #{poke pjsua_config, cb.on_incoming_call}

setOnMediaStateCallback
    :: Ptr PjSuaConfig -> FunPtr OnMediaStateHandler -> PjIO ()
setOnMediaStateCallback =
    (liftIO .) . #{poke pjsua_config, cb.on_call_media_state}

setOnRegistrationStartedCallback
    :: Ptr PjSuaConfig -> FunPtr OnRegistrationStartedHandler -> PjIO ()
setOnRegistrationStartedCallback =
    (liftIO .) . #{poke pjsua_config, cb.on_reg_started}

setOnRegistrationStateCallback
    :: Ptr PjSuaConfig -> FunPtr OnRegistrationStateHandler -> PjIO ()
setOnRegistrationStateCallback =
    (liftIO .) . #{poke pjsua_config, cb.on_reg_state}
