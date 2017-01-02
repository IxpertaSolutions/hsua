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
    , getMsg
    )
  where

import Foreign.Ptr (Ptr)

import Phone.Internal.FFI.Common (PjIO(PjIO))
import Phone.Internal.FFI.Msg(Msg)

#include <pjsua-lib/pjsua.h>

data RxData

foreign import ccall "get_rx_msg" getMsg :: Ptr RxData -> PjIO (Ptr Msg)
