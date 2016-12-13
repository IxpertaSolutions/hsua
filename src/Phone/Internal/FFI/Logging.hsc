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
    , setLevel
    , setLogFilename
    , setMsgLogging
    , withLoggingConfig
    )
  where

#include <pjsua-lib/pjsua.h>

import Data.Function (($), (.))
import Foreign.C.Types (CInt, CUInt)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (pokeByteOff)

import Control.Monad.IO.Class (liftIO)

import Phone.Internal.FFI.Common (PjIO(PjIO), liftAlloc)
import Phone.Internal.FFI.PjString (PjString)


data LoggingConfig

withLoggingConfig :: (Ptr LoggingConfig -> PjIO a) -> PjIO a
withLoggingConfig f =
    liftAlloc (allocaBytes #{size pjsua_logging_config}) $ \cfg -> do
        defaultLoggingConfig cfg
        f cfg

foreign import ccall "pjsua_logging_config_default" defaultLoggingConfig
    :: Ptr LoggingConfig -> PjIO ()

setConsoleLevel :: Ptr LoggingConfig -> CUInt -> PjIO ()
setConsoleLevel = (liftIO .) . #{poke pjsua_logging_config, console_level}

setLevel :: Ptr LoggingConfig -> CUInt -> PjIO ()
setLevel = (liftIO .) . #{poke pjsua_logging_config, level}

setMsgLogging :: Ptr LoggingConfig -> CInt -> PjIO ()
setMsgLogging = (liftIO .) . #{poke pjsua_logging_config, msg_logging}

setLogFilename :: Ptr LoggingConfig -> PjString -> PjIO ()
setLogFilename = (liftIO .) . #{poke pjsua_logging_config, log_filename}
