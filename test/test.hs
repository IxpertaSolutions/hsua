{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Control.Concurrent
import System.IO

import Phone.Internal.FFI
import Phone.Internal.FFI.Common
import Phone.Internal.FFI.Transport
import Phone.Internal.FFI.Media
import Phone.Internal.FFI.Account
import Phone.Internal.FFI.PjString
import Phone.Internal.FFI.CallManipulation
import Phone.Internal.FFI.Configuration

incomingCallHandler :: OnIncomingCallHandler
incomingCallHandler _ callId _ = do
    res <- callAnswer callId 200 nullPtr nullPtr
    putStrLn $ "call accept result: " <> show res

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
    mediaCfg <- createMediaConfig
    defaultMedaiConfig mediaCfg
    initializePjSua pjCfg nullPtr mediaCfg

    -- Initialize transport
    transportCfg <- createTransportConfig
    -- setPort transportCfg 5060
    createTransport udpTransport transportCfg nullPtr >>= (putStrLn . show)
    pjsuaStart
    putStrLn "****************************************"
    printDevices
    putStrLn "****************************************"

    -- Create account
    accCfg <- createAccountConfig
    defaultAccountConfig accCfg
    newCString "sip:420123456789@192.168.0.30" >>= createPjString >>= setAccoutId accCfg
    newCString "sip:192.168.0.30" >>= createPjString >>= setAccountRegUri accCfg
    setAccountCredCount accCfg 1
    newCString "*" >>= createPjString >>= setAccountRealm accCfg 0
    newCString "digest" >>= createPjString >>= setAccountSchema accCfg 0
    newCString "420123456789" >>= createPjString >>= setAccountUsername accCfg 0
    setAccountDataType accCfg 0 credDataPlainPasswd
    newCString "123" >>= createPjString >>= setAccountData accCfg 0
    accountId <- malloc
    setAccount accCfg pjTrue accountId
    --setNullSndDev

    threadDelay 1000000
    id' <- peek accountId
    dst <- newCString "sip:420123456788@192.168.0.30" >>= createPjString
    makeCall id' dst nullPtr nullPtr nullPtr nullPtr >>= (putStrLn . show)

    threadDelay 10000000
    hanhupAll
    destroyPjSua >>= (putStrLn . show)
