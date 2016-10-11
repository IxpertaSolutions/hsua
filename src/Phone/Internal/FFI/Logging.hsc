{-# LANGUAGE ForeignFunctionInterface #-}
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

import Control.Monad
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

data LoggingConfig

foreign import ccall "create_pjsua_logging_config" createLoggingConfig
    :: IO (Ptr LoggingConfig)
foreign import ccall "pjsua_logging_config_default" defaultLoggingConfig
    :: Ptr LoggingConfig -> IO ()
