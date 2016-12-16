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
    ( FFI.Event
    , FFI.EventType
    , getHeaderFromEvent
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing))
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Foreign.C.String (peekCStringLen)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)

import qualified Phone.Internal.FFI.Event as FFI
    ( Event
    , EventType
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
    ( getHeaderVelue
    , pjSipMsgFindHeaderByName
    )
import qualified Phone.Internal.FFI.PjString as FFI
    ( stringLenFromPjString
    , withPjStringPtr
    )
import qualified Phone.Internal.FFI.RxData as FFI (getMsgInfo)
import Phone.MonadPJ (MonadPJ(liftPJ))


getHeaderFromEvent :: MonadPJ m => Ptr FFI.Event -> Text -> m (Maybe Text)
getHeaderFromEvent ev hName = liftPJ $ FFI.getEventType ev >>= \case
    FFI.Unknown -> pure Nothing
    FFI.Timer -> pure Nothing
    FFI.TxMsg -> pure Nothing
    FFI.RxMsg -> FFI.withPjStringPtr (T.unpack hName) $
        \hName' ->  FFI.pjSipMsgFindHeaderByName
            (FFI.getMsgInfo $ FFI.getRxData ev) hName' nullPtr >>= maybeValue
    FFI.TransportError -> pure Nothing
    FFI.TransactionState -> pure Nothing
    FFI.User -> pure Nothing
  where
    maybeValue res = if res == nullPtr
        then pure Nothing
        else (Just . T.pack) <$> liftIO (peek (FFI.getHeaderVelue res)
            >>= (peekCStringLen . FFI.stringLenFromPjString))
