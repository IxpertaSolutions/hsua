{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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
module Phone.Internal.FFI.PjString
    ( PjString
    , AsPjString(..)
    , peekPjStringPtr
    , withPjStringPtr
    )
  where

#include <pjsua-lib/pjsua.h>

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

import Prelude (fromIntegral)

import Control.Applicative((<*>))
import Control.Monad ((=<<))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.String (String)
import Foreign.C.String (CString, CStringLen, peekCStringLen, withCStringLen)
import Foreign.C.Types (CLong)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Foreign.Storable
    ( Storable(alignment, sizeOf, peek, poke)
    , peekByteOff
    , pokeByteOff
    )

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.Foreign as T (peekCStringLen, withCStringLen)

import Phone.Internal.FFI.Common (PjIO, liftAlloc)


data PjString = PjString CString CLong

instance Storable PjString where
    sizeOf _ = #{size pj_str_t}
    alignment _ = #{alignment pj_str_t}
    peek ptr = PjString
        <$> #{peek pj_str_t, ptr} ptr
        <*> #{peek pj_str_t, slen} ptr
    poke ptr (PjString p l) = do
        #{poke pj_str_t, ptr} ptr p
        #{poke pj_str_t, slen} ptr l

class AsPjString s where
    withPjString :: s -> (PjString -> PjIO a) -> PjIO a
    peekPjString :: PjString -> PjIO s

instance AsPjString String where
    withPjString str f =
        liftAlloc (withCStringLen str) $ f . pjStringFromCStringLen
    peekPjString = liftIO . peekCStringLen . cStringLenFromPjString

instance AsPjString Text where
    withPjString str f =
        liftAlloc (T.withCStringLen str) $ f . pjStringFromCStringLen
    peekPjString = liftIO . T.peekCStringLen . cStringLenFromPjString

pjStringFromCStringLen :: CStringLen -> PjString
pjStringFromCStringLen (p, l) = PjString p (fromIntegral l)

cStringLenFromPjString :: PjString -> CStringLen
cStringLenFromPjString (PjString p l) = (p, fromIntegral l)

withPjStringPtr :: AsPjString s => s -> (Ptr PjString -> PjIO a) -> PjIO a
withPjStringPtr str f = withPjString str $ \pjStr -> liftAlloc (with pjStr) f

peekPjStringPtr :: AsPjString s => Ptr PjString -> PjIO s
peekPjStringPtr p = peekPjString =<< liftIO (peek p)
