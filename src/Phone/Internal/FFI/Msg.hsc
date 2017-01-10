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
    , getHdrName
    , getMsgHdr
    , getNextHdr
    , printHdrToCString
    )
  where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((.))
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(CInt))
import Foreign.Storable (peekByteOff)
import System.IO (IO)

import Phone.Internal.FFI.Common (PjIO)
import Phone.Internal.FFI.PjString (PjString)

#include <pjsua-lib/pjsua.h>


data Msg
data Hdr

getMsgHdr :: Ptr Msg -> Ptr Hdr
getMsgHdr = #{ptr pjsip_msg, hdr}

getNextHdr :: Ptr Hdr -> PjIO (Ptr Hdr)
getNextHdr = liftIO . #{peek pjsip_hdr, next}

getHdrName :: Ptr Hdr -> PjIO PjString
getHdrName = liftIO . #{peek pjsip_hdr, name}

foreign import ccall "pjsip_hdr_print_on" printHdrToCString
    :: Ptr Hdr -> CString -> CInt -> IO CInt
