{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Data.Function (($))
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid ((<>))
import System.IO (IO, putStrLn)
import Text.Show (show)

import Phone.Account
    ( Account
        ( Account
        )
    , AccountId
    , AuthScheme
        ( Digest
        )
    , WhenRegister
        ( Now
        )
    , accountId
    , authScheme
    , createAccount
    , isAccountRegistered
    , password
    , realm
    , registrationUri
    , userName
    )
import Phone.Call (CallId, answerCall, hangupAll, makeCall)
import Phone.Handlers
    ( Handlers
        ( Handlers
        )
    , onCallStateChange
    , onIncomingCall
    , onMediaStateChange
    , onRegistrationStarted
    , onRegistrationStateChange
    )
import Phone.Run (setNullSndDev, withPhone)


incomingCallHandler :: AccountId -> CallId -> IO ()
incomingCallHandler _ callId = do
    res <- answerCall callId 200
    putStrLn $ "call accept result: " <> show res

onRegistrationHandler :: AccountId -> IO ()
onRegistrationHandler id = do
    r <- isAccountRegistered id
    putStrLn $ "is account registred: " <> show r

main :: IO ()
main = withPhone handlers $ do
    accId <- createAccount Now Account
        { accountId = "sip:123@10.0.0.1"
        , registrationUri = "sip:10.0.0.1"
        , realm = "*"
        , authScheme = Digest
        , userName = "123"
        , password = "*****"
        }

    threadDelay 1000000
    setNullSndDev
    putStrLn "creating call"
    _ <- makeCall accId "sip:1234@10.0.0.2"

    threadDelay 10000000
    hangupAll
  where
    handlers = Handlers
        { onCallStateChange = Nothing
        , onIncomingCall = Just incomingCallHandler
        , onRegistrationStateChange = Just onRegistrationHandler
        , onRegistrationStarted = Nothing
        , onMediaStateChange = Nothing
        }
