{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Module:       $HEADER$
-- Description:  Low level FFI.
-- Copyright:
-- License:      GPL-2
--
-- Maintainer:   Jan Sipr <jan.sipr@ixperta.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
module Phone.Run
    ( deinitPhone
    , initPhone
    , setNullSndDev
    , withPhone
    )
  where

import Prelude (fromIntegral)

import Control.Applicative (pure)
import Control.Exception (bracket_)
import Control.Monad ((>=>), (>>=), void)
import Data.Function (($), (.))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr (nullPtr)
import System.IO (IO)

import Control.Monad.IO.Class (liftIO)

import Phone.Config
    ( Config
        ( Config
        , handlers
        , logging
        )
    , Handlers
        ( Handlers
        , onCallStateChange
        , onCallTransactionStateChange
        , onIncomingCall
        , onMediaStateChange
        , onRegistrationStarted
        , onRegistrationStateChange
        )
    , Logging
        ( Logging
        , logLevel
        , logConsoleLevel
        , logMsgLogging
        , logFilename
        )
    )
import Phone.Exception
    ( PhoneException
        ( CreateLib
        , Initialization
        , Start
        , Transport
        )
    )
import Phone.MonadPJ (MonadPJ(liftPJ))
import Phone.Internal.Event (toEvent)
import qualified Phone.Internal.FFI as FFI
    ( createPjSua
    , destroyPjSua
    , pjsuaStart
    , setNullSndDev
    , codecSetPriority
    )
import qualified Phone.Internal.FFI.CallManipulation as FFI (hangupAll)
import qualified Phone.Internal.FFI.Configuration as FFI
    ( initializePjSua
    , setOnCallStateCallback
    , setOnCallTransactionStateCallback
    , setOnIncomingCallCallback
    , setOnMediaStateCallback
    , setOnRegistrationStartedCallback
    , setOnRegistrationStateCallback
    , toOnCallState
    , toOnCallTransactioState
    , toOnIncomingCall
    , toOnMediaState
    , toOnRegistrationStarted
    , toOnRegistrationState
    , withPjConfig
    )
import qualified Phone.Internal.FFI.Logging as FFI
    ( setConsoleLevel
    , setLevel
    , setLogFilename
    , setMsgLogging
    , withLoggingConfig
    )
import qualified Phone.Internal.FFI.Media as FFI
    ( withMediaConfig
    , setMediaConfigClockRate
    )
import qualified Phone.Internal.FFI.PjString as FFI
    ( withPjString
    , withPjStringPtr
    )
import qualified Phone.Internal.FFI.Transport as FFI
    ( createTransport
    , udpTransport
    , withTransportConfig
    )
import qualified Phone.Internal.Utils as FFI (check)

withPhone :: Config -> IO () -> IO ()
withPhone cfg = bracket_ (initPhone cfg) deinitPhone

initPhone :: Config -> IO ()
initPhone Config{..} = liftPJ $ do
    FFI.createPjSua >>= FFI.check CreateLib
    FFI.withPjConfig $ \pjCfg -> do
        whenJust onCallStateChange
            $ liftIO . FFI.toOnCallState . onCallState
            >=> FFI.setOnCallStateCallback pjCfg
        whenJust onCallTransactionStateChange
            $ liftIO . FFI.toOnCallTransactioState . onCallTransactionState
            >=> FFI.setOnCallTransactionStateCallback pjCfg
        whenJust onIncomingCall
            $ liftIO . FFI.toOnIncomingCall . onIncCall
            >=> FFI.setOnIncomingCallCallback pjCfg
        whenJust onRegistrationStateChange
            $ liftIO . FFI.toOnRegistrationState
            >=> FFI.setOnRegistrationStateCallback pjCfg
        whenJust onRegistrationStarted
            $ liftIO . FFI.toOnRegistrationStarted . onRegStarted
            >=> FFI.setOnRegistrationStartedCallback pjCfg
        whenJust onMediaStateChange
            $ liftIO . FFI.toOnMediaState
            >=> FFI.setOnMediaStateCallback pjCfg
        withLog $ \logCfg ->
            withMedia
                $ FFI.initializePjSua pjCfg logCfg
                >=> FFI.check Initialization
    FFI.withTransportConfig $ \transportCfg ->
        FFI.createTransport FFI.udpTransport transportCfg nullPtr
        >>= FFI.check Transport
    FFI.pjsuaStart >>= FFI.check Start
    setCodecs
  where
    Handlers{..} = handlers
    Logging{..} = logging

    withLog f =
        withMaybePjString logFilename $ \logFile ->
        FFI.withLoggingConfig $ \logCfg -> do
            whenJust logMsgLogging $ FFI.setMsgLogging logCfg . fromBool
            whenJust logLevel $ FFI.setLevel logCfg . fromIntegral
            whenJust logConsoleLevel $ FFI.setConsoleLevel logCfg . fromIntegral
            whenJust logFile $ FFI.setLogFilename logCfg
            f logCfg

    withMedia f =
        FFI.withMediaConfig $ \mediaCfg -> do
            -- When pjproject is built without resampling (as it happens to be
            -- in Debian), we want to fix the rate and codecs so that we don't
            -- crash in resampler creation.
            FFI.setMediaConfigClockRate mediaCfg 8000
            f mediaCfg

    setCodecs = do
        FFI.withPjStringPtr "PCMU" $ \codecStr ->
            FFI.codecSetPriority codecStr 255
        FFI.withPjStringPtr "PCMA" $ \codecStr ->
            FFI.codecSetPriority codecStr 255

    onCallState f callId event =
        toEvent event >>= f callId

    onCallTransactionState f callId _ event =
        toEvent event >>= f callId

    onIncCall f acc callId _ = f acc callId

    onRegStarted f acc p = f acc $ fromIntegral p

    whenJust m op = maybe (pure ()) op m

    withMaybePjString = maybe ($ Nothing) ((. (. Just)) . FFI.withPjString)

deinitPhone :: IO ()
deinitPhone = liftPJ $ do
    FFI.hangupAll
    void FFI.destroyPjSua

setNullSndDev :: MonadPJ m => m ()
setNullSndDev = liftPJ FFI.setNullSndDev
