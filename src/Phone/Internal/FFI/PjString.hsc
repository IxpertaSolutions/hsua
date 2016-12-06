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
    ( createPjString
    , deletePjString
    , setPjString
    )
  where

#include <pjsua-lib/pjsua.h>

import Foreign.C.String (CString)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr)
import System.IO (IO)

import Phone.Internal.FFI.Common (PjString)

foreign import ccall "create_pj_str" createPjString
    :: CString -> IO (Ptr PjString)
foreign import ccall "delete_pj_str" deletePjString
    :: Ptr PjString -> IO ()

setPjString :: Ptr PjString -> Ptr PjString -> IO ()
setPjString dst src = copyBytes dst src #{size pj_str_t}
