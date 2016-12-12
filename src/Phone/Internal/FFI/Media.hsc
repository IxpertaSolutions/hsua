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
    ( MediaConfig
    , setMediaConfigClockRate
    , withMediaConfig
    )
  where

#include <pjsua-lib/pjsua.h>

import Data.Function (($))
import Foreign.C.Types (CUInt)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import System.IO (IO)


data MediaConfig

withMediaConfig :: (Ptr MediaConfig -> IO a) -> IO a
withMediaConfig f = allocaBytes #{size pjsua_media_config} $ \cfg -> do
    defaultMediaConfig cfg
    f cfg

foreign import ccall "pjsua_media_config_default" defaultMediaConfig
    :: Ptr MediaConfig -> IO ()

setMediaConfigClockRate :: Ptr MediaConfig -> CUInt -> IO ()
setMediaConfigClockRate = #{poke pjsua_media_config, clock_rate}
