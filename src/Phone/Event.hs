{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module:       $HEADER$
-- Description:  Low level FFI.
-- Copyright:
-- License:      GPL-2
--
-- Maintainer:   Jan Sipr <jan.sipr@ixperta.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
module Phone.Event
    ( FFI.EventType
    , Event
    , getHeaderFromEvent
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Function (($), (.))
import Data.Maybe (Maybe(Nothing))
import Foreign.Ptr (nullPtr)

import Data.Text (Text)
import qualified Data.Text as T (unpack)

import Phone.Internal.Event (Event(Event))
import qualified Phone.Internal.FFI.Common as FFI (maybePeek)
import qualified Phone.Internal.FFI.Event as FFI
    ( EventType
        ( Unknown
        , Timer
        , TxMsg
        , RxMsg
        , TransportError
        , TransactionState
        , User
        )
    , getEventType
    , getRxData
    )
import qualified Phone.Internal.FFI.GenericStringHeader as FFI
    ( getHeaderValue
    , msgFindHeaderByName
    )
import qualified Phone.Internal.FFI.PjString as FFI
    ( peekPjStringPtr
    , withPjStringPtr
    )
import qualified Phone.Internal.FFI.RxData as FFI (getMsgInfo)
import Phone.MonadPJ (MonadPJ(liftPJ))


getHeaderFromEvent :: MonadPJ m => Event -> Text -> m (Maybe Text)
getHeaderFromEvent (Event ev) hName = liftPJ $ FFI.getEventType ev >>= \case
    FFI.Unknown -> pure Nothing
    FFI.Timer -> pure Nothing
    FFI.TxMsg -> pure Nothing
    FFI.RxMsg -> msgFindHeaderByName hName >>= maybeValue
    FFI.TransportError -> pure Nothing
    FFI.TransactionState -> pure Nothing
    FFI.User -> pure Nothing
  where
    maybeValue = FFI.maybePeek (FFI.peekPjStringPtr . FFI.getHeaderValue)

    msgInfo = FFI.getMsgInfo $ FFI.getRxData ev

    msgFindHeaderByName name =
        FFI.withPjStringPtr (T.unpack name) $ \namePjStr ->
            FFI.msgFindHeaderByName msgInfo namePjStr nullPtr
