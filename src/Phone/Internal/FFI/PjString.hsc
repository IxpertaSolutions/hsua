{-# LANGUAGE ForeignFunctionInterface #-}
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
  where

#include <pjsua-lib/pjsua.h>

import Control.Monad
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import Text.Show (Show)

import Phone.Internal.FFI.Common

foreign import ccall "create_pj_str" createPjString
    :: CString -> IO (Ptr PjString)
foreign import ccall "delete_pj_str" deletePjString
    :: Ptr PjString -> IO ()

