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
module Phone.Internal.FFI.MsgData
    ( MsgData
    , pushHeader
    , withMsgData
    )
  where

#include <pjsua-lib/pjsua.h>

import Data.Function (($), (.))
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Marshal.Alloc (allocaBytes)

import Phone.Internal.FFI.Common (PjIO(PjIO), liftAlloc)
import Phone.Internal.FFI.GenericStringHeader (GenericStringHeader)


data MsgData
data HeaderList

withMsgData :: (Ptr MsgData -> PjIO a) -> PjIO a
withMsgData f =
    liftAlloc (allocaBytes #{size pjsua_msg_data}) $ \msgData -> do
        initMsgData msgData
        f msgData

foreign import ccall "pjsua_msg_data_init" initMsgData
    :: Ptr MsgData -> PjIO ()

foreign import ccall "pj_list_insert_before" pjListInsertBefore
    :: Ptr HeaderList -> Ptr GenericStringHeader -> PjIO ()

pushHeader :: Ptr MsgData -> Ptr GenericStringHeader -> PjIO ()
pushHeader = pjListInsertBefore . #{ptr pjsua_msg_data, hdr_list}
