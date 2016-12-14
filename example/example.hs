{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Data.Function (($), (.))
import Data.Maybe (Maybe(Just))
import Data.Monoid ((<>))
import System.IO (IO, putStrLn)
import Text.Show (show)

import Control.Monad.IO.Class (liftIO)
import Data.Default (def)

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
import Phone.Config
    ( Config(handlers)
    , onIncomingCall
    , onRegistrationStateChange
    )
import Phone.MonadPJ (PjIO)
import Phone.Run (setNullSndDev, withPhone)


incomingCallHandler :: AccountId -> CallId -> PjIO ()
incomingCallHandler _ callId = do
    res <- answerCall callId 200
    liftIO . putStrLn $ "call accept result: " <> show res

onRegistrationHandler :: AccountId -> PjIO ()
onRegistrationHandler id = do
    r <- isAccountRegistered id
    liftIO . putStrLn $ "is account registred: " <> show r

main :: IO ()
main = withPhone config $ do
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
    config = def
        { handlers = def
            { onIncomingCall = Just incomingCallHandler
            , onRegistrationStateChange = Just onRegistrationHandler
            }
        }
