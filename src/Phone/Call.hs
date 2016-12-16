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
    ( FFI.CallId
    , FFI.CallState(..)
    , CallInfo(..)
    , answerCall
    , hangupAll
    , hangupCall
    , makeCall
    , makeCallWithHeaders
    , getCallInfo
    )
  where

import Prelude (fromIntegral)

import Control.Applicative (Applicative((<*>)))
import Control.Monad ((>>=), mapM_)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (reverse)
import Data.Tuple (uncurry)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Text.Show (Show)

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T (unpack)

import Phone.Exception
    ( PhoneException
        ( AnswerCall
        , GetCallInfo
        , HangupCall
        , MakeCall
        )
    )
import Phone.MonadPJ (MonadPJ(liftPJ))
import qualified Phone.Internal.FFI.Account as FFI (AccountId)
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
import qualified Phone.Internal.FFI.Common as FFI (CallId, PjIO, liftAlloc)
import qualified Phone.Internal.FFI.MsgData as FFI
    ( MsgData
    , pushHeader
    , withMsgData
    )
import qualified Phone.Internal.FFI.PjString as FFI (withPjStringPtr)
import qualified Phone.Internal.FFI.GenericStringHeader as FFI (withHeader)
import qualified Phone.Internal.Utils as FFI (check)


data CallInfo = CallInfo
    { accountId :: FFI.AccountId
    , callState :: FFI.CallState
    }
  deriving (Show)

getCallInfo :: MonadPJ m => FFI.CallId -> m CallInfo
getCallInfo callId = liftPJ . FFI.withCallInfo $ \info -> do
    FFI.getCallInfo callId info >>= FFI.check GetCallInfo
    CallInfo <$> FFI.getAccountId info <*> FFI.getCallState info

answerCall
    :: MonadPJ m
    => FFI.CallId
    -> Int
    -- ^ Status code to be used to answer the call.
    -> m ()
answerCall callId status = liftPJ $
    FFI.answerCall callId (fromIntegral status) nullPtr nullPtr
    >>= FFI.check AnswerCall

hangupCall
    :: MonadPJ m
    => FFI.CallId
    -> Int
    -- ^ Status code to be used to answer the call.
    -> m ()
hangupCall callId status = liftPJ $
    FFI.hangupCall callId (fromIntegral status) nullPtr nullPtr
    >>= FFI.check HangupCall

makeCall :: MonadPJ m => FFI.AccountId -> Text -> m FFI.CallId
makeCall accId url = liftPJ $
    FFI.withPjStringPtr (T.unpack url) $ \urlPjStr ->
    FFI.liftAlloc alloca $ \callId -> do
        FFI.makeCall accId urlPjStr nullPtr nullPtr nullPtr callId
            >>= FFI.check MakeCall
        liftIO $ peek callId

makeCallWithHeaders
    :: MonadPJ m
    => FFI.AccountId
    -> Text
    -> [(Text, Text)]
    -> m FFI.CallId
makeCallWithHeaders accId url hs = liftPJ $
    FFI.withPjStringPtr (T.unpack url) $ \urlPjStr ->
    withMsgData hs $ \msgData ->
    FFI.liftAlloc alloca $ \callId -> do
        FFI.makeCall accId urlPjStr nullPtr nullPtr msgData callId
            >>= FFI.check MakeCall
        liftIO $ peek callId

withMsgData :: [(Text, Text)] -> (Ptr FFI.MsgData -> FFI.PjIO a) -> FFI.PjIO a
withMsgData headers f = go headers []
  where
    go [] ps = withMsgData' (reverse ps) f
    go (h:hs) ps = uncurry withHeader h $ \p -> go hs (p:ps)

    withMsgData' ptrs g =
        FFI.withMsgData $ \msgData -> do
            mapM_ (FFI.pushHeader msgData) ptrs
            g msgData

    withHeader name value g =
        FFI.withPjStringPtr (T.unpack name) $ \namePjStr ->
        FFI.withPjStringPtr (T.unpack value) $ \valuePjStr ->
        FFI.withHeader namePjStr valuePjStr g

hangupAll :: MonadPJ m => m ()
hangupAll = liftPJ FFI.hangupAll
