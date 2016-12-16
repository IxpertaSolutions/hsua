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
module Phone.Internal.FFI.RxData
    ( RxData
    , getMsgInfo
    )
  where

import Foreign.Ptr (Ptr, plusPtr)

import Phone.Internal.FFI.Common(Msg)

#include <pjsua-lib/pjsua.h>

data RxData

getMsgInfo :: Ptr RxData -> Ptr Msg
getMsgInfo = #{ptr pjsip_rx_data, msg_info.msg}
