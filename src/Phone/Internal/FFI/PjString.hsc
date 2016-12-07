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
    , withPjString
    , withPjStringPtr
    )
  where

#include <pjsua-lib/pjsua.h>

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

import Prelude (fromIntegral)

import Control.Applicative((<*>))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.String (String)
import Foreign.C.String (CString, CStringLen, withCStringLen)
import Foreign.C.Types (CLong)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Foreign.Storable
    ( Storable(alignment, sizeOf, peek, poke)
    , peekByteOff
    , pokeByteOff
    )
import System.IO (IO)


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

pjStringFromCStringLen :: CStringLen -> PjString
pjStringFromCStringLen (p, l) = PjString p (fromIntegral l)

withPjString :: String -> (PjString -> IO a) -> IO a
withPjString str f = withCStringLen str $ f . pjStringFromCStringLen

withPjStringPtr :: String -> (Ptr PjString -> IO a) -> IO a
withPjStringPtr str f = withPjString str (`with` f)
