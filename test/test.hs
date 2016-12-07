{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad ((>>=))
import Data.Function (($))
import Data.Monoid ((<>))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import System.IO
    (BufferMode(NoBuffering), IO, hSetBuffering, print, putStrLn, stdout)
import Text.Show (show)

import Phone.Internal.FFI
    ( createPjSua
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
import Phone.Internal.FFI.Common (pjSuccess, pjTrue)
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
import Phone.Internal.FFI.PjString
    ( withPjString
    , withPjStringPtr
    )
import Phone.Internal.FFI.Transport
    ( createTransport
    , udpTransport
    , withTransportConfig
    )

incomingCallHandler :: OnIncomingCallHandler
incomingCallHandler _ callId _ = do
    res <- answerCall callId 200 nullPtr nullPtr
    putStrLn $ "call accept result: " <> show res

onRegistrationHandler :: OnRegistrationStateHandler
onRegistrationHandler id = do
    putStrLn "#####################################################"
    r <- isAccountRegistered id
    putStrLn $ "is account registred: " <> show r
    putStrLn "#####################################################"

onMediaState :: OnMediaStateHandler
onMediaState _ =
    putStrLn "Media state handler!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    createPjSua >>= print
    putStrLn $ "pjTrue: " <> show pjTrue
    putStrLn $ "pjSuccess: " <> show pjSuccess
    -- Initialize pjsua lib.
    _ <- withPjConfig $ \pjCfg -> do
        toOnIncomingCall incomingCallHandler >>= setOnIncomingCallCallback pjCfg
        toOnMediaState onMediaState >>= setOnMediaStateCallback pjCfg
        toOnRegistrationState onRegistrationHandler
            >>= setOnRegistrationStateCallback pjCfg
        initializePjSua pjCfg nullPtr nullPtr

    -- Initialize transport
    withTransportConfig $ \transportCfg -> do
        -- setPort transportCfg 5060
        createTransport udpTransport transportCfg nullPtr >>= print
    _ <- pjsuaStart
    putStrLn "****************************************"
    printDevices
    putStrLn "****************************************"

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
            alloca $ \accountId -> do
                _ <- setAccount accCfg pjTrue accountId
                peek accountId

    setNullSndDev

    threadDelay 1000000
    withPjStringPtr "sip:420242492306@10.120.51.51" $ \dstPjStr ->
        makeCall accountId dstPjStr nullPtr nullPtr nullPtr nullPtr >>= print

    threadDelay 10000000
    hangupAll
    destroyPjSua >>= print
