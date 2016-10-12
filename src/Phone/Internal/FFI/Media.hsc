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
module Phone.Internal.FFI.Media
  where

import Foreign.Ptr (Ptr)

import System.IO (IO)

data MediaConfig

foreign import ccall "create_pjsua_media_config" createMediaConfig
    :: IO (Ptr MediaConfig)
foreign import ccall "pjsua_media_config_default" defaultMedaiConfig
    :: Ptr MediaConfig -> IO ()

