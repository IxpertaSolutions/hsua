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
    )
  where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((.))
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekByteOff)

import Phone.Internal.FFI.Common (PjIO(PjIO))
import Phone.Internal.FFI.PjString (PjString)

#include <pjsua-lib/pjsua.h>

data Msg
data Hdr
data HdrVptr


foreign import ccall "get_msg_hdr" getMsgHdr :: Ptr Msg -> PjIO (Ptr Hdr)
foreign import ccall "get_msg_next_hdr" getNextHdr :: Ptr Hdr -> PjIO (Ptr Hdr)

getHdrName :: Ptr Hdr -> PjIO PjString
getHdrName = liftIO . #{peek pjsip_hdr, name}

-- This is where we can get vale (`print_on` can give it into our buffer).
getHdrVptr :: Ptr Hdr -> Ptr HdrVptr
getHdrVptr = #{ptr pjsip_hdr, next}
