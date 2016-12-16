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
module Phone.Internal.Event
    ( Event(..)
    )
  where

import Foreign.Ptr (Ptr)

import qualified Phone.Internal.FFI.Event as FFI (Event)


data Event = Event (Ptr FFI.Event)
