{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Internal utility functions.
-- Copyright:
-- License:      GPL-2
--
-- Maintainer:   Jan Sipr <jan.sipr@ixperta.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
module Phone.Internal.Utils
    ( check
    )
  where

import Control.Applicative (pure)
import Control.Exception.Base (Exception, throwIO)
import Data.Eq ((/=))
import System.IO (IO)

import Phone.Internal.FFI.Common (PjStatus, pjSuccess)


check :: Exception e => e -> PjStatus -> IO ()
check e ret = if ret /= pjSuccess
    then throwIO e
    else pure ()
