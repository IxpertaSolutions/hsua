{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
module Phone.Internal.FFI.Msg
    ( Msg
    , Hdr
    , getMsgHdr
    , getNextHdr
    , getHdrName
    , getHdrVptr
    , getMsgHdrListLength
    )
  where

import Foreign.Ptr (Ptr, plusPtr)
import Foreign.C.Types (CInt(CInt))
import Foreign.Storable (peekByteOff)
import System.IO (IO)

import Phone.Internal.FFI.Common (PjIO(PjIO))
import Phone.Internal.FFI.PjString (PjString)

#include <pjsua-lib/pjsua.h>

data Msg
data Hdr
data HdrVptr


foreign import ccall "get_msg_hdr" getMsgHdr :: Ptr Msg -> PjIO (Ptr Hdr)
foreign import ccall "get_msg_hdr_list_len" getMsgHdrListLength :: Ptr Msg -> PjIO CInt
foreign import ccall "get_msg_next_hdr" getNextHdr :: Ptr Hdr -> PjIO (Ptr Hdr)

-- getNextHdr :: Ptr Hdr -> Ptr Hdr
-- getNextHdr = #{ptr pjsip_hdr, next}

getHdrName :: Ptr Hdr -> IO PjString
getHdrName = #{peek pjsip_hdr, name}

-- This is where we can get vale (`print_on` can give it into our buffer).
getHdrVptr :: Ptr Hdr -> Ptr HdrVptr
getHdrVptr = #{ptr pjsip_hdr, next}
