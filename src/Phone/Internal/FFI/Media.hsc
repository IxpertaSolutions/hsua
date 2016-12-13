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

import Data.Function (($), (.))
import Foreign.C.Types (CUInt)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)

import Control.Monad.IO.Class (liftIO)

import Phone.Internal.FFI.Common (PjIO(PjIO), liftAlloc)


data MediaConfig

withMediaConfig :: (Ptr MediaConfig -> PjIO a) -> PjIO a
withMediaConfig f =
    liftAlloc (allocaBytes #{size pjsua_media_config}) $ \cfg -> do
        defaultMediaConfig cfg
        f cfg

foreign import ccall "pjsua_media_config_default" defaultMediaConfig
    :: Ptr MediaConfig -> PjIO ()

setMediaConfigClockRate :: Ptr MediaConfig -> CUInt -> PjIO ()
setMediaConfigClockRate = (liftIO .) . #{poke pjsua_media_config, clock_rate}
