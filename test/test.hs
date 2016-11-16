{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad ((>>=))
import Data.Function (($))
import Data.Monoid ((<>))
import Foreign.C.String (newCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (malloc)
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
    ( createAccountConfig
    , credDataPlainPasswd
    , defaultAccountConfig
    , isAccountRegistered
    , setAccount
    , setAccountCredCount
    , setAccountData
    , setAccountDataType
    , setAccountRealm
    , setAccountRegUri
    , setAccountScheme
    , setAccountUsername
    , setAccoutId
    )
import Phone.Internal.FFI.CallManipulation (answerCall, hangupAll, makeCall)
import Phone.Internal.FFI.Common (pjSuccess, pjTrue)
import Phone.Internal.FFI.Configuration
    ( OnIncomingCallHandler
    , OnRegistrationStateHandler
    , createPjConfig
    , defaultPjConfig
    , initializePjSua
    , setOnIncomingCallCallback
    , setOnMediaStateCallback
    , setOnRegistrationStateCallback
    , toOnIncomingCall
    , toOnMediaState
    , toOnRegistrationState
    )
import Phone.Internal.FFI.Media (createMediaConfig, defaultMedaiConfig)
import Phone.Internal.FFI.PjString (createPjString, deletePjString)
import Phone.Internal.FFI.Transport
    ( createTransport
    , createTransportConfig
    , udpTransport
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

onMediaState :: CInt -> IO ()
onMediaState _ =
    putStrLn "Media state handler!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    createPjSua >>= print
    str <- newCString "asdflfs" >>= createPjString
    deletePjString str
    putStrLn $ "pjTrue: " <> show pjTrue
    putStrLn $ "pjSuccess: " <> show pjSuccess
    -- Initialize pjsua lib.
    pjCfg <- createPjConfig
    defaultPjConfig pjCfg
    toOnIncomingCall incomingCallHandler >>= setOnIncomingCallCallback pjCfg
    toOnMediaState onMediaState >>= setOnMediaStateCallback pjCfg
    toOnRegistrationState onRegistrationHandler
        >>= setOnRegistrationStateCallback pjCfg
    mediaCfg <- createMediaConfig
    defaultMedaiConfig mediaCfg
    _ <- initializePjSua pjCfg nullPtr mediaCfg

    -- Initialize transport
    transportCfg <- createTransportConfig
    -- setPort transportCfg 5060
    createTransport udpTransport transportCfg nullPtr >>= print
    _ <- pjsuaStart
    putStrLn "****************************************"
    printDevices
    putStrLn "****************************************"

    -- Create account
    accCfg <- createAccountConfig
    defaultAccountConfig accCfg
    newCString "sip:420242492304@10.120.51.51" >>= createPjString
        >>= setAccoutId accCfg
    newCString "sip:10.120.51.51" >>= createPjString >>= setAccountRegUri accCfg
    setAccountCredCount accCfg 1
    newCString "*" >>= createPjString >>= setAccountRealm accCfg 0
    newCString "digest" >>= createPjString >>= setAccountScheme accCfg 0
    newCString "420242492304" >>= createPjString >>= setAccountUsername accCfg 0
    setAccountDataType accCfg 0 credDataPlainPasswd
    newCString "420242492304" >>= createPjString >>= setAccountData accCfg 0
    accountId <- malloc
    _ <- setAccount accCfg pjTrue accountId
    setNullSndDev

    threadDelay 1000000
    id' <- peek accountId
    dst <- newCString "sip:420242492306@10.120.51.51" >>= createPjString
    makeCall id' dst nullPtr nullPtr nullPtr nullPtr >>= print

    threadDelay 10000000
    hangupAll
    destroyPjSua >>= print
