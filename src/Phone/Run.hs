{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
    ( withPhone
    , setNullSndDev
    )
  where

import Prelude (fromIntegral)

import Control.Applicative (pure)
import Control.Exception (bracket_)
import Control.Monad ((>=>), (>>=), void)
import Data.Function (($), (.))
import Data.Maybe (maybe)
import Foreign.Ptr (nullPtr)
import System.IO (IO)

import Control.Monad.IO.Class (liftIO)

import Phone.Exception
    ( PhoneException
        ( CreateLib
        , Initialization
        , Start
        , Transport
        )
    )
import Phone.Handlers
    ( Handlers
        ( Handlers
        , onCallStateChange
        , onIncomingCall
        , onMediaStateChange
        , onRegistrationStarted
        , onRegistrationStateChange
        )
    )
import Phone.MonadPJ (MonadPJ(liftPJ))
import qualified Phone.Internal.FFI as FFI
    ( createPjSua
    , destroyPjSua
    , pjsuaStart
    , setNullSndDev
    , codecSetPriority
    )
import qualified Phone.Internal.FFI.CallManipulation as FFI (hangupAll)
import qualified Phone.Internal.FFI.Common as FFI (pjFalse)
import qualified Phone.Internal.FFI.Configuration as FFI
    ( initializePjSua
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
import qualified Phone.Internal.FFI.Logging as FFI
    ( withLoggingConfig
    , setLogFilename
    , setMsgLogging
    -- , setConsoleLevel
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

withPhone :: Handlers -> IO () -> IO ()
withPhone Handlers{..} = bracket_ initSeq deinitSeq
  where
    initSeq = liftPJ $ do
        FFI.createPjSua >>= FFI.check CreateLib
        FFI.withPjConfig $ \pjCfg -> do
            maybeHandler onCallStateChange
                (liftIO . FFI.toOnCallState . onCallState
                >=> FFI.setOnCallStateCallback pjCfg)
            maybeHandler onIncomingCall
                (liftIO . FFI.toOnIncomingCall . onIncCall
                >=> FFI.setOnIncomingCallCallback pjCfg)
            maybeHandler onRegistrationStateChange
                (liftIO . FFI.toOnRegistrationState
                >=> FFI.setOnRegistrationStateCallback pjCfg)
            maybeHandler onRegistrationStarted
                (liftIO . FFI.toOnRegistrationStarted . onRegStarted
                >=> FFI.setOnRegistrationStartedCallback pjCfg)
            maybeHandler onMediaStateChange
                (liftIO . FFI.toOnMediaState
                >=> FFI.setOnMediaStateCallback pjCfg)
            withLog $ \logCfg ->
                withMedia $ \mediaCfg ->
                    FFI.initializePjSua pjCfg logCfg mediaCfg
                    >>= FFI.check Initialization
        FFI.withTransportConfig $ \transportCfg ->
            FFI.createTransport FFI.udpTransport transportCfg nullPtr
            >>= FFI.check Transport
        FFI.pjsuaStart >>= FFI.check Start
        setCodecs

    withLog f =
        FFI.withPjString "pjsua_log.txt" $ \logFile -> -- FIXME: hardcoded
        FFI.withLoggingConfig $ \logCfg -> do
            FFI.setMsgLogging logCfg FFI.pjFalse
            FFI.setLogFilename logCfg logFile
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

    onCallState f callId _ = f callId
    onIncCall f acc callId _ = f acc callId
    onRegStarted f acc p = f acc $ fromIntegral p

    deinitSeq = liftPJ $ do
        FFI.hangupAll
        void FFI.destroyPjSua

    maybeHandler m op = maybe (pure ()) op m

setNullSndDev :: MonadPJ m => m ()
setNullSndDev = liftPJ FFI.setNullSndDev
