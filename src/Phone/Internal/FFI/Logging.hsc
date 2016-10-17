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
module Phone.Internal.FFI.Logging
  where

import Foreign.C.Types (CUInt(CUInt))
import Foreign.Ptr (Ptr)

import System.IO (IO)

data LoggingConfig

foreign import ccall "create_pjsua_logging_config" createLoggingConfig
    :: IO (Ptr LoggingConfig)

foreign import ccall "pjsua_logging_config_default" defaultLoggingConfig
    :: Ptr LoggingConfig -> IO ()

foreign import ccall "pjsua_logging_set_console_level" setConsoleLevel
    :: Ptr LoggingConfig -> CUInt -> IO ()
