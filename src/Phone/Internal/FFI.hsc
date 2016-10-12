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
module Phone.Internal.FFI
  where

-- GHC lower than 8.0 don't have alignment macro.
#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

-- This allows to retrieve value from enums and defines
-- Pjsua uses extremely tricky enums...
#let enumToValue t = "%d", (int)t

#include <pjsua-lib/pjsua.h>

import Foreign.C.Types (CInt(CInt))
import Foreign.C.String (CString)

import System.IO (IO)

import Phone.Internal.FFI.Common (PjStatus)

-- | Calls createPjSua which load the pjsua library in to memory.
--
-- __Must be called before any another hsua (pjsua) function is called.__
foreign import ccall "pjsua_create" createPjSua :: IO PjStatus

-- | Opposite function to 'createPjSua' function. It destroys the hsua (pjsua) library
-- memory representaiton.
--
-- __No hsua (pjsua) function may be called after this function.__
foreign import ccall "pjsua_destroy" destroyPjSua :: IO PjStatus

foreign import ccall "pjsua_start" pjsuaStart :: IO PjStatus

foreign import ccall "pjsua_verify_sip_url" verifySipUrl
    :: CString -> IO PjStatus

foreign import ccall "pjsua_verify_url" verifyTelUrl
    :: CString -> IO PjStatus

foreign import ccall "pjsua_set_null_snd_dev" setNullSndDev :: IO ()
foreign import ccall "print_devices" printDevices :: IO ()
