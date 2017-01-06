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
import Data.Eq ((==), (/=))
import Data.Ord ((>))
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.List (drop, dropWhile)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.String (String)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Foreign.C.String (peekCStringLen)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
import System.IO (IO)
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
    FFI.RxMsg -> FFI.getMsgFromEvent evPtr
        >>= FFI.getMsgHdr
        >>= fmap RxMsg . toHeaderList
    FFI.TransportError -> pure TransportError
    FFI.TransactionState -> FFI.getTsxType evPtr >>= \case
        FFI.RxMsg -> FFI.getTsxRxData evPtr
            >>= FFI.getMsg
            >>= FFI.getMsgHdr
            >>= fmap TransactionState . toHeaderList
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
                    >>= go ((mk name, T.pack value) : list)

getHdrValue :: Ptr FFI.Hdr -> FFI.PjIO String
getHdrValue hdr = liftIO $ dropHeaderName <$> getHdr' 512 -- Arbitrary size :D
  where
    getHdr' :: Int -> IO String
    getHdr' size = do
        val <- allocaBytes size $ \cstring -> do
            res <- FFI.printHdrToCString hdr cstring (fromIntegral size)
            -- printHdrToCString return -1 if the cstring array is to small
            if res == -1
                then pure Nothing
                else Just <$> peekCStringLen (cstring, fromIntegral res)
        maybe (cycle size) pure val

    cycle size = let a = size * 2 in
        if a > 25536
            then getHdr' $ a
            else fail "SIP header is to long..."

    dropHeaderName = dropWhile (' ' ==) . drop 1 . dropWhile (':' /=)
