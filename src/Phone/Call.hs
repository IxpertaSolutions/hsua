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

import Control.Applicative (Applicative((<*>), pure))
import Control.Monad (forM, mapM_)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Foreign.Ptr (Ptr, nullPtr)
import Text.Show (Show)

import Control.Monad.Trans.Cont (ContT(ContT, runContT), evalContT)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)

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
import qualified Phone.Internal.FFI.Common as FFI (CallId, PjIO)
import qualified Phone.Internal.FFI.MsgData as FFI
    ( MsgData
    , pushHeader
    , withMsgData
    )
import qualified Phone.Internal.FFI.PjString as FFI (withPjStringPtr)
import qualified Phone.Internal.FFI.GenericStringHeader as FFI (withHeader)
import qualified Phone.Internal.Utils as FFI (check, checkPeek)


data CallInfo = CallInfo
    { accountId :: FFI.AccountId
    , callState :: FFI.CallState
    }
  deriving (Show)

getCallInfo :: MonadPJ m => FFI.CallId -> m CallInfo
getCallInfo callId = liftPJ . FFI.withCallInfo $ \info -> do
    FFI.check GetCallInfo $ FFI.getCallInfo callId info
    CallInfo <$> FFI.getAccountId info <*> FFI.getCallState info

answerCall
    :: MonadPJ m
    => FFI.CallId
    -> Int
    -- ^ Status code to be used to answer the call.
    -> m ()
answerCall callId status = liftPJ . FFI.check AnswerCall
    $ FFI.answerCall callId (fromIntegral status) nullPtr nullPtr

hangupCall
    :: MonadPJ m
    => FFI.CallId
    -> Int
    -- ^ Status code to be used to answer the call.
    -> m ()
hangupCall callId status = liftPJ . FFI.check HangupCall
    $ FFI.hangupCall callId (fromIntegral status) nullPtr nullPtr

makeCall :: MonadPJ m => FFI.AccountId -> Text -> m FFI.CallId
makeCall accId url = liftPJ . evalContT $ do
    urlPjStr <- ContT $ FFI.withPjStringPtr url
    lift . FFI.checkPeek MakeCall
        $ FFI.makeCall accId urlPjStr nullPtr nullPtr nullPtr

makeCallWithHeaders
    :: MonadPJ m
    => FFI.AccountId
    -> Text
    -> [(Text, Text)]
    -> m FFI.CallId
makeCallWithHeaders accId url hs = liftPJ . evalContT $ do
    urlPjStr <- ContT $ FFI.withPjStringPtr url
    msgData <- ContT $ withMsgData hs
    lift . FFI.checkPeek MakeCall
        $ FFI.makeCall accId urlPjStr nullPtr nullPtr msgData

withMsgData :: [(Text, Text)] -> (Ptr FFI.MsgData -> FFI.PjIO a) -> FFI.PjIO a
withMsgData headers = runContT $ do
    ptrs <- forM headers $ \(name, value) -> do
        namePjStr <- ContT $ FFI.withPjStringPtr name
        valuePjStr <- ContT $ FFI.withPjStringPtr value
        ContT $ FFI.withHeader namePjStr valuePjStr
    msgData <- ContT FFI.withMsgData
    mapM_ (lift . FFI.pushHeader msgData) ptrs
    pure msgData

hangupAll :: MonadPJ m => m ()
hangupAll = liftPJ FFI.hangupAll
