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
module Phone.Call
    ( CallId
    , FFI.CallState(..)
    , CallInfo(..)
    , answerCall
    , FFI.hangupAll
    , hangupCall
    , makeCall
    , getCallInfo
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Function (($))
import Data.Int (Int)
import Data.Text (Text, unpack)
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import Prelude (fromIntegral)
import System.IO (IO)
import Text.Show (Show)

import Phone.Exception
    ( PhoneException
        ( AnswerCall
        , GetCallInfo
        , HangupCall
        , MakeCall
        )
    )
import Phone.Internal.FFI.Account (AccountId)
import qualified Phone.Internal.FFI.CallInfo as FFI
    ( CallState
        ( Calling
        , Confirmed
        , Connecting
        , Disconnected
        , EarlyMedia
        , Incoming
        , Null
        )
    , createCallInfo
    , getAccountId
    , getCallInfo
    , getCallState
    )
import qualified Phone.Internal.FFI.CallManipulation as FFI
    ( answerCall
    , hangupAll
    , hangupCall
    , makeCall
    )
import Phone.Internal.FFI.Common (CallId)
import Phone.Internal.FFI.PjString (createPjString)
import Phone.Internal.Utils (check)


data CallInfo = CallInfo
    { accountId :: AccountId
    , callState :: FFI.CallState
    }
  deriving (Show)

getCallInfo :: CallId -> IO CallInfo
getCallInfo callId = do
    -- TODO: There is batter way to handle allocation and deallocation
    info <- FFI.createCallInfo
    FFI.getCallInfo callId info >>= check GetCallInfo
    accId <- FFI.getAccountId info
    state <- FFI.getCallState info
    free info
    pure $ CallInfo accId state

answerCall
    :: CallId
    -> Int
    -- ^ Status code to be used to answer the call.
    -> IO ()
answerCall callId status =
    FFI.answerCall callId (fromIntegral status) nullPtr nullPtr
    >>= check AnswerCall

hangupCall
    :: CallId
    -> Int
    -- ^ Status code to be used to answer the call.
    -> IO ()
hangupCall callId status =
    FFI.hangupCall callId (fromIntegral status) nullPtr nullPtr
    >>= check HangupCall

makeCall :: AccountId -> Text -> IO CallId
makeCall accId url = do
    dst <- newCString (unpack url) >>= createPjString
    callId <- malloc
    FFI.makeCall accId dst nullPtr nullPtr nullPtr callId >>= check MakeCall
    res <- peek callId
    -- TODO: There is batter way to handle allocation and deallocation
    free callId
    pure res
