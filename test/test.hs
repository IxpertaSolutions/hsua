{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad ((>>=))
import Data.Function (($), (.))
import Data.Monoid ((<>))
import Foreign.C.String (newCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (malloc)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import System.IO (BufferMode (NoBuffering), IO, hSetBuffering, putStrLn, stdout)
import Text.Show (show)

import Phone.Internal.FFI (createPjSua, destroyPjSua, pjsuaStart, printDevices)
import Phone.Internal.FFI.Account
    ( createAccountConfig
    , credDataPlainPasswd
    , defaultAccountConfig
    , isAccoutRegistred
    , setAccount
    , setAccountCredCount
    , setAccountData
    , setAccountDataType
    , setAccountRealm
    , setAccountRegUri
    , setAccountSchema
    , setAccountUsername
    , setAccoutId
    )
import Phone.Internal.FFI.CallManipulation (callAnswer, hanhupAll, makeCall)
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
    res <- callAnswer callId 200 nullPtr nullPtr
    putStrLn $ "call accept result: " <> show res

onRegistrationHandler :: OnRegistrationStateHandler
onRegistrationHandler id = do
    putStrLn "#####################################################"
    r <- isAccoutRegistred id
    putStrLn $ "is account registred: " <> show r
    putStrLn "#####################################################"

onMediaState :: CInt -> IO ()
onMediaState _ =
    putStrLn "Media state handler!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    createPjSua >>= (putStrLn . show)
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
    createTransport udpTransport transportCfg nullPtr >>= (putStrLn . show)
    _ <- pjsuaStart
    putStrLn "****************************************"
    printDevices
    putStrLn "****************************************"

    -- Create account
    accCfg <- createAccountConfig
    defaultAccountConfig accCfg
    newCString "sip:420123456789@192.168.0.30" >>= createPjString
        >>= setAccoutId accCfg
    newCString "sip:192.168.0.30" >>= createPjString >>= setAccountRegUri accCfg
    setAccountCredCount accCfg 1
    newCString "*" >>= createPjString >>= setAccountRealm accCfg 0
    newCString "digest" >>= createPjString >>= setAccountSchema accCfg 0
    newCString "420123456789" >>= createPjString >>= setAccountUsername accCfg 0
    setAccountDataType accCfg 0 credDataPlainPasswd
    newCString "123" >>= createPjString >>= setAccountData accCfg 0
    accountId <- malloc
    _ <- setAccount accCfg pjTrue accountId
    --setNullSndDev

    threadDelay 1000000
    id' <- peek accountId
    dst <- newCString "sip:420123456788@192.168.0.30" >>= createPjString
    makeCall id' dst nullPtr nullPtr nullPtr nullPtr >>= (putStrLn . show)

    threadDelay 10000000
    hanhupAll
    destroyPjSua >>= (putStrLn . show)
