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
module Phone.Call
    ( CallId
    , answerCall
    , FFI.hangupAll
    , hangupCall
    , makeCall
    )
  where

import Control.Applicative (pure)
import Control.Exception.Base (Exception, throwIO)
import Control.Monad ((>>=))
import Data.Eq ((/=))
import Data.Int (Int)
import Data.Text (Text, unpack)
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (malloc)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import Prelude (fromIntegral)
import System.IO (IO)

import Phone.Exception (PhoneException (AnswerCall, HangupCall, MakeCall))
import Phone.Internal.FFI.Account (AccountId)
import qualified Phone.Internal.FFI.CallManipulation as FFI
    ( answerCall
    , hangupAll
    , hangupCall
    , makeCall
    )
import Phone.Internal.FFI.Common (CallId, PjStatus, pjSuccess)
import Phone.Internal.FFI.PjString (createPjString)

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
    peek callId


check :: Exception e => e -> PjStatus -> IO ()
check e ret = if ret /= pjSuccess
    then throwIO e
    else pure ()
