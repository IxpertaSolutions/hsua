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
    ( LoggingConfig
    , setConsoleLevel
    , withLoggingConfig
    )
  where

#include <pjsua-lib/pjsua.h>

import Data.Function (($))
import Foreign.C.Types (CUInt)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (pokeByteOff)

import System.IO (IO)

data LoggingConfig

withLoggingConfig :: (Ptr LoggingConfig -> IO a) -> IO a
withLoggingConfig f = allocaBytes #{size pjsua_logging_config} $ \cfg -> do
    defaultLoggingConfig cfg
    f cfg

foreign import ccall "pjsua_logging_config_default" defaultLoggingConfig
    :: Ptr LoggingConfig -> IO ()

setConsoleLevel :: Ptr LoggingConfig -> CUInt -> IO ()
setConsoleLevel = #{poke pjsua_logging_config, console_level}
