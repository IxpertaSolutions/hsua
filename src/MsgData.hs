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
module Phone.Internal.FFI.MsgData

import Data.Function (($), (.))
import Foreign.C.Types (CInt, CUInt)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (pokeByteOff)

import Control.Monad.IO.Class (liftIO)

import Phone.Internal.FFI.Common (PjIO(PjIO), liftAlloc)
import Phone.Internal.FFI.PjString (PjString)
