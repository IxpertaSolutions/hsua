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
module Phone.Internal.FFI.Transport
  where

import Foreign.C.Types (CInt(CInt), CUInt(CUInt))
import Foreign.Ptr (Ptr)

import System.IO (IO)

import Phone.Internal.FFI.Common (PjStatus)

#include <pjsua-lib/pjsua.h>

data TransportConfig
data TransportId

foreign import ccall "create_pjsua_transport_config" createTransportConfig
    :: IO (Ptr TransportConfig)

foreign import ccall "pjsua_transport_config_set_port" setPort
    :: Ptr TransportConfig -> CUInt -> IO ()

udpTransport :: CInt
udpTransport = #{const PJSIP_TRANSPORT_UDP}

tcpTransport :: CInt
tcpTransport = #{const PJSIP_TRANSPORT_TCP}

foreign import ccall "pjsua_transport_create" createTransport
    :: CInt -> Ptr TransportConfig -> Ptr TransportId -> IO PjStatus

-- data TransportType =
--     Unspecified
--     | UDP
--     | TCP
--     | TLS
--     | SCTP
--     | LOOP
--     | LOOP_DGRAM
--     | START_OTHER
--     | IPV6
--     | UDP6
--     | TCP6
--     | TLS6
--   deriving (Show, Eq)
--
-- instance Enum TransportType where
--   fromEnum Unspecified = 0
--   fromEnum UDP = 1
--   fromEnum TCP = 2
--
--   toEnum 0 = PlainKey
--   toEnum 1 = SpecialKey
--   toEnum 2 = NoKey
--   toEnum unmatched = error ("Key.toEnum: Cannot match " ++ show unmatched)
