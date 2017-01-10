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

import Control.Monad.IO.Class (liftIO)
import Data.Function ((.))
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)

import Phone.Internal.FFI.Common (PjIO)
import Phone.Internal.FFI.Msg (Msg)

#include <pjsua-lib/pjsua.h>

data RxData

getMsg :: Ptr RxData -> PjIO (Ptr Msg)
getMsg = liftIO . #{peek pjsip_rx_data, msg_info.msg}
