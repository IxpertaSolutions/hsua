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

import Prelude ((*), Int, fromIntegral)

import Control.Applicative ((<$>), pure)
import Control.Monad ((>>=), fail)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (otherwise)
import Data.Eq ((==))
import Data.Ord ((>=))
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Tuple (snd)
import qualified Data.Text as T (breakOn, stripPrefix, unpack)
import qualified Data.Text.Foreign as T (peekCStringLen)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
import System.IO (IO)
import Text.Show (Show)

import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI (mk)

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
    , getRxRxData
    , getTsxRxData
    , getTsxType
    )
import qualified Phone.Internal.FFI.Common as FFI (PjIO)
import qualified Phone.Internal.FFI.Msg as FFI
    ( Hdr
    , getHdrName
    , getMsgHdr
    , getNextHdr
    , printHdrToCString
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
    FFI.RxMsg -> FFI.getRxRxData evPtr
        >>= FFI.getMsg
        >>= fmap RxMsg . toHeaderList . FFI.getMsgHdr
    FFI.TransportError -> pure TransportError
    FFI.TransactionState -> FFI.getTsxType evPtr >>= \case
        FFI.RxMsg -> FFI.getTsxRxData evPtr
            >>= FFI.getMsg
            >>= fmap TransactionState . toHeaderList . FFI.getMsgHdr
        _ -> pure $ TransactionState []
    FFI.User -> pure User
  where
    toHeaderList :: Ptr FFI.Hdr -> FFI.PjIO [(CI Text, Text)]
    toHeaderList hdr = FFI.getNextHdr hdr >>= go []
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
                name <- FFI.getHdrName hdr' >>= FFI.peekPjString
                value <- getHdrValue hdr'
                FFI.getNextHdr hdr'
                    >>= go ((CI.mk name, value) : list)

getHdrValue :: Ptr FFI.Hdr -> FFI.PjIO Text
getHdrValue hdr = liftIO $ go 512 >>= dropHeaderName -- Arbitrary size :D
  where
    go :: Int -> IO Text
    go size
        | size >= 32768 = fail "SIP header is to long..."
        | otherwise = getHdr size >>= maybe (go (size * 2)) pure

    getHdr :: Int -> IO (Maybe Text)
    getHdr size = allocaBytes size $ \cstring -> do
        res <- FFI.printHdrToCString hdr cstring (fromIntegral size)
        -- printHdrToCString returns -1 if the cstring array is to small
        if res == -1
            then pure Nothing
            else Just <$> T.peekCStringLen (cstring, fromIntegral res)

    dropHeaderName h = case T.stripPrefix ": " . snd . T.breakOn ": " $ h of
        Just v -> pure v
        Nothing -> fail . T.unpack $ "Malformed header: " <> h
