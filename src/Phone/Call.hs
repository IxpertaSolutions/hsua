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

import Control.Applicative (Applicative((<*>)))
import Control.Monad ((>>=))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Text (Text, unpack)
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (alloca)
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
    , getAccountId
    , getCallInfo
    , getCallState
    , withCallInfo
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
getCallInfo callId = FFI.withCallInfo $ \info -> do
    FFI.getCallInfo callId info >>= check GetCallInfo
    CallInfo <$> FFI.getAccountId info <*> FFI.getCallState info

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
    alloca $ \callId -> do
        FFI.makeCall accId dst nullPtr nullPtr nullPtr callId >>= check MakeCall
        peek callId
