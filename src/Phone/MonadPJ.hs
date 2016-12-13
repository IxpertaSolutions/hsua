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
module Phone.MonadPJ
    ( MonadPJ(liftPJ)
    , PjIO
    )
  where

import Control.Monad (Monad)
import Data.Function (id)
import System.IO (IO)

import Phone.Internal.FFI.Common (PjIO, runPjIO)


class Monad m => MonadPJ m where
    liftPJ :: PjIO a -> m a

instance MonadPJ IO where
    liftPJ = runPjIO

instance MonadPJ PjIO where
    liftPJ = id
