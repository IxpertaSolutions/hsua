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
    ( codecSetPriority
    , createPjSua
    , destroyPjSua
    , pjsuaStart
    , printDevices
    , setNullSndDev
    , verifySipUrl
    , verifyTelUrl
    )
  where

#include <pjsua-lib/pjsua.h>

import Foreign.C.Types (CInt(CInt), CUChar(CUChar))
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

import Phone.Internal.FFI.Common (PjIO(PjIO), PjStatus(PjStatus))
import Phone.Internal.FFI.PjString (PjString)


-- | Calls createPjSua which load the pjsua library in to memory.
--
-- __Must be called before any another hsua (pjsua) function is called.__
foreign import ccall "pjsua_create" createPjSua :: PjIO PjStatus

-- | Opposite function to 'createPjSua' function. It destroys the hsua (pjsua) library
-- memory representaiton.
--
-- __No hsua (pjsua) function may be called after this function.__
foreign import ccall "pjsua_destroy" destroyPjSua :: PjIO PjStatus

foreign import ccall "pjsua_start" pjsuaStart :: PjIO PjStatus

foreign import ccall "pjsua_verify_sip_url" verifySipUrl
    :: CString -> PjIO PjStatus

foreign import ccall "pjsua_verify_url" verifyTelUrl
    :: CString -> PjIO PjStatus

foreign import ccall "pjsua_set_null_snd_dev" setNullSndDev :: PjIO ()

foreign import ccall "pjsua_codec_set_priority" codecSetPriority
    :: Ptr PjString -> CUChar -> PjIO ()

foreign import ccall "print_devices" printDevices :: PjIO ()
