{-# LANGUAGE ForeignFunctionInterface #-}
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
module Phone.Internal.FFI.Transport
    ( TransportConfig
    , createTransport
    , setTransportPort
    , tcpTransport
    , udpTransport
    , withTransportConfig
    )
  where

#include <pjsua-lib/pjsua.h>

import Data.Function (($), (.))
import Foreign.C.Types (CInt(CInt), CUInt)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, pokeByteOff)

import Control.Monad.IO.Class (liftIO)

import Phone.Internal.FFI.Common (PjIO(PjIO), PjStatus(PjStatus), liftAlloc)


data TransportConfig
data TransportId
newtype TransportType = TransportType CInt deriving Storable

withTransportConfig :: (Ptr TransportConfig -> PjIO a) -> PjIO a
withTransportConfig f =
    liftAlloc (allocaBytes #{size pjsua_transport_config}) $ \cfg -> do
        defaultTransportConfig cfg
        f cfg

setTransportPort :: Ptr TransportConfig -> CUInt -> PjIO ()
setTransportPort = (liftIO .) . #{poke pjsua_transport_config, port}

#{enum TransportType, TransportType,
    udpTransport = PJSIP_TRANSPORT_UDP,
    tcpTransport = PJSIP_TRANSPORT_TCP
}

foreign import ccall "pjsua_transport_config_default" defaultTransportConfig
    :: Ptr TransportConfig -> PjIO ()

foreign import ccall "pjsua_transport_create" createTransport
    :: TransportType -> Ptr TransportConfig -> Ptr TransportId -> PjIO PjStatus
