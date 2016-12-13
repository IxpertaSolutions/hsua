{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad ((>>=))
import Data.Function (($), (.))
import Data.Monoid ((<>))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import System.IO
    (BufferMode(NoBuffering), IO, hSetBuffering, print, putStrLn, stdout)
import Text.Show (show)

import Control.Monad.IO.Class (liftIO)

import Phone.Internal.FFI
    ( createPjSua
    , codecSetPriority
    , destroyPjSua
    , pjsuaStart
    , printDevices
    , setNullSndDev
    )
import Phone.Internal.FFI.Account
    ( credDataPlainPasswd
    , isAccountRegistered
    , setAccount
    , setAccountCredCount
    , setAccountData
    , setAccountDataType
    , setAccountRealm
    , setAccountRegUri
    , setAccountScheme
    , setAccountUsername
    , setAccountId
    , withAccountConfig
    )
import Phone.Internal.FFI.CallManipulation (answerCall, hangupAll, makeCall)
import Phone.Internal.FFI.Common (pjSuccess, pjTrue, pjFalse, runPjIO, liftAlloc)
import Phone.Internal.FFI.Configuration
    ( OnIncomingCallHandler
    , OnMediaStateHandler
    , OnRegistrationStateHandler
    , initializePjSua
    , setOnIncomingCallCallback
    , setOnMediaStateCallback
    , setOnRegistrationStateCallback
    , toOnIncomingCall
    , toOnMediaState
    , toOnRegistrationState
    , withPjConfig
    )
import Phone.Internal.FFI.Logging
    ( withLoggingConfig
    , setLogFilename
    , setMsgLogging
    -- , setConsoleLevel
    )
import Phone.Internal.FFI.PjString
    ( withPjString
    , withPjStringPtr
    )
import Phone.Internal.FFI.Media (withMediaConfig, setMediaConfigClockRate)
import Phone.Internal.FFI.Transport
    ( createTransport
    , udpTransport
    , withTransportConfig
    )

incomingCallHandler :: OnIncomingCallHandler
incomingCallHandler _ callId _ = do
    res <- answerCall callId 200 nullPtr nullPtr
    liftIO $ putStrLn $ "call accept result: " <> show res

onRegistrationHandler :: OnRegistrationStateHandler
onRegistrationHandler id = do
    liftIO $ putStrLn "#####################################################"
    r <- isAccountRegistered id
    liftIO $ putStrLn $ "is account registred: " <> show r
    liftIO $ putStrLn "#####################################################"

onMediaState :: OnMediaStateHandler
onMediaState _ =
    liftIO $ putStrLn "Media state handler!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

main :: IO ()
main = runPjIO $ do
    liftIO $ hSetBuffering stdout NoBuffering
    createPjSua >>= liftIO . print
    liftIO . putStrLn $ "pjTrue: " <> show pjTrue
    liftIO . putStrLn $ "pjSuccess: " <> show pjSuccess

    let withLog f =
            withPjString "pjsua_log.txt" $ \logFile ->
            withLoggingConfig $ \logCfg -> do
                setMsgLogging logCfg pjFalse
                setLogFilename logCfg logFile
                f logCfg
    let withMedia f =
            withMediaConfig $ \mediaCfg -> do
                setMediaConfigClockRate mediaCfg 8000
                f mediaCfg

    -- Initialize pjsua lib.
    _ <- withPjConfig $ \pjCfg -> do
        liftIO (toOnIncomingCall incomingCallHandler)
            >>= setOnIncomingCallCallback pjCfg
        liftIO (toOnMediaState onMediaState)
            >>= setOnMediaStateCallback pjCfg
        liftIO (toOnRegistrationState onRegistrationHandler)
            >>= setOnRegistrationStateCallback pjCfg
        withLog $ \logCfg ->
            withMedia $ \mediaCfg ->
                initializePjSua pjCfg logCfg mediaCfg

    withPjStringPtr "PCMU" $ \codecStr -> codecSetPriority codecStr 255
    withPjStringPtr "PCMA" $ \codecStr -> codecSetPriority codecStr 255

    -- Initialize transport
    withTransportConfig $ \transportCfg -> do
        -- setPort transportCfg 5060
        createTransport udpTransport transportCfg nullPtr >>= liftIO . print
    _ <- pjsuaStart
    liftIO $ putStrLn "****************************************"
    printDevices
    liftIO $ putStrLn "****************************************"

    -- Create account
    accountId <-
        withPjString "sip:420242492304@10.120.51.51" $ \accountIdPjStr ->
        withPjString "sip:10.120.51.51" $ \registrationUriPjStr ->
        withPjString "*" $ \realmPjStr ->
        withPjString "digest" $ \schemePjStr ->
        withPjString "420242492304" $ \userNamePjStr ->
        withPjString "420242492304" $ \passwordPjStr ->
        withAccountConfig $ \accCfg -> do
            setAccountId accCfg accountIdPjStr
            setAccountRegUri accCfg registrationUriPjStr
            setAccountCredCount accCfg 1
            setAccountRealm accCfg 0 realmPjStr
            setAccountScheme accCfg 0 schemePjStr
            setAccountUsername accCfg 0 userNamePjStr
            setAccountDataType accCfg 0 credDataPlainPasswd
            setAccountData accCfg 0 passwordPjStr
            liftAlloc alloca $ \accountId -> do
                _ <- setAccount accCfg pjTrue accountId
                liftIO $ peek accountId

    setNullSndDev

    liftIO $ threadDelay 1000000
    withPjStringPtr "sip:420242492306@10.120.51.51" $ \dstPjStr ->
        makeCall accountId dstPjStr nullPtr nullPtr nullPtr nullPtr >>= liftIO . print

    liftIO $ threadDelay 10000000
    hangupAll
    destroyPjSua >>= liftIO . print
