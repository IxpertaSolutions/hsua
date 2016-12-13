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
import Control.Monad.IO.Class (liftIO)
import Data.Eq ((/=))

import Phone.Internal.FFI.Common (PjIO, PjStatus, pjSuccess)


check :: Exception e => e -> PjStatus -> PjIO ()
check e ret = if ret /= pjSuccess
    then liftIO (throwIO e)
    else pure ()
