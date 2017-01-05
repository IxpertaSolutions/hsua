{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
module Phone.Internal.Event
    ( Event(..)
    , toEvent
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (otherwise)
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Foreign.Ptr (Ptr)
import Text.Show (Show)

import Data.CaseInsensitive (CI, mk)

import qualified Phone.Internal.FFI.Event as FFI
    ( Event
    , EventType
        ( RxMsg
        , Timer
        , TransactionState
        , TransportError
        , TxMsg
        , Unknown
        , User
        )
    , getEventType
    , getMsgFromEvent
    , getTsxRxData
    , getTsxType
    )
import qualified Phone.Internal.FFI.Common as FFI (PjIO)
import qualified Phone.Internal.FFI.Msg as FFI
    ( Hdr
    , getHdrName
    , getMsgHdr
    , getNextHdr
    )
import qualified Phone.Internal.FFI.PjString as FFI (peekPjString)
import qualified Phone.Internal.FFI.RxData as FFI (getMsg)


data Event
    = Unknown
    | Timer
    | TxMsg
    | RxMsg ![(CI Text, Text)]
    | TransportError
    | TransactionState ![(CI Text, Text)]
    | User
  deriving (Show)

toEvent :: Ptr FFI.Event -> FFI.PjIO Event
toEvent evPtr = FFI.getEventType evPtr >>= \case
    FFI.Unknown -> pure Unknown
    FFI.Timer -> pure Timer
    FFI.TxMsg -> pure TxMsg
    FFI.RxMsg -> FFI.getMsgFromEvent evPtr
        >>= FFI.getMsgHdr
        >>= fmap RxMsg . magic
    FFI.TransportError -> pure TransportError
    FFI.TransactionState -> FFI.getTsxType evPtr >>= \case
        FFI.RxMsg -> FFI.getTsxRxData evPtr
            >>= FFI.getMsg
            >>= FFI.getMsgHdr
            >>= fmap TransactionState . magic
        _ -> pure $ TransactionState []
    FFI.User -> pure User
  where
    magic :: Ptr FFI.Hdr -> FFI.PjIO [(CI Text, Text)]
    magic hdr = FFI.getNextHdr hdr >>= go []
      where
        endHdr :: Ptr FFI.Hdr
        endHdr = hdr

        go
            :: [(CI Text, Text)]
            -> Ptr FFI.Hdr
            -> FFI.PjIO [(CI Text, Text)]
        go list hdr'
            | hdr' == endHdr = pure list
            | otherwise = do
                namePjString <- FFI.getHdrName hdr'
                name <- FFI.peekPjString namePjString
                -- TODO: Fill the header value
                FFI.getNextHdr hdr' >>= go ((mk $ T.pack name, "") : list)
