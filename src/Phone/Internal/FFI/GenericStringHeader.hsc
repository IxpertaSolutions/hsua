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
module Phone.Internal.FFI.GenericStringHeader
    ( GenericStringHeader
    , getHeaderName
    , getHeaderValue
    , msgFindHeaderByName
    , withHeader
    )
  where

#include <pjsua-lib/pjsua.h>

import Data.Function (($))
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Marshal.Alloc (allocaBytes)

import Phone.Internal.FFI.Common (PjIO(PjIO), Msg, liftAlloc)
import Phone.Internal.FFI.PjString (PjString)


data GenericStringHeader

foreign import ccall "pjsip_generic_string_hdr_init2" initHeader
    :: Ptr GenericStringHeader -> Ptr PjString -> Ptr PjString -> PjIO ()

withHeader
    :: Ptr PjString
    -> Ptr PjString
    -> (Ptr GenericStringHeader -> PjIO a)
    -> PjIO a
withHeader hNamef hValue f =
    liftAlloc (allocaBytes #{size pjsip_generic_string_hdr}) $ \stringHdr -> do
        initHeader stringHdr hNamef hValue
        f stringHdr

foreign import ccall "pjsip_msg_find_hdr_by_name" msgFindHeaderByName
    :: Ptr Msg
    -> Ptr PjString
    -> Ptr GenericStringHeader
    -> PjIO (Ptr GenericStringHeader)

getHeaderName :: Ptr GenericStringHeader -> Ptr PjString
getHeaderName = #{ptr pjsip_generic_string_hdr, name}

getHeaderValue :: Ptr GenericStringHeader -> Ptr PjString
getHeaderValue = #{ptr pjsip_generic_string_hdr, hvalue}
