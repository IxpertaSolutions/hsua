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
    , checkPeek
    )
  where

import Control.Exception.Base (Exception, throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Eq ((==))
import Data.Function (($))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, peek)

import Phone.Internal.FFI.Common (PjIO, PjStatus, liftAlloc, pjSuccess)


check :: Exception e => e -> PjIO PjStatus -> PjIO ()
check e action = do
    ret <- action
    unless (ret == pjSuccess) $ liftIO (throwIO e)

checkPeek
    :: (Exception e, Storable a) => e -> (Ptr a -> PjIO PjStatus) -> PjIO a
checkPeek e action =
    liftAlloc alloca $ \ptr -> do
        check e (action ptr)
        liftIO (peek ptr)
