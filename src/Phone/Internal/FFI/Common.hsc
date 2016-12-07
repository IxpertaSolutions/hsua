{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
module Phone.Internal.FFI.Common
  where

import Data.Eq (Eq)
import Foreign.C.Types (CInt)
import Foreign.Storable (Storable)

#include <pjsua-lib/pjsua.h>

import Text.Show (Show)


data CallSetting
data MsgData
data Reason
data SipEvent
data UserData
data MediaConfig

newtype CallId = CallId CInt deriving (Show, Storable)
newtype PjStatus = PjStatus CInt deriving (Eq, Show, Storable)

-- | Equivalent to PJ_TRUE
pjTrue :: CInt
pjTrue = #{const PJ_TRUE}

-- | Equivalent to PJ_FALSE
pjFalse :: CInt
pjFalse = #{const PJ_FALSE}

#{enum PjStatus, PjStatus, pjSuccess = PJ_SUCCESS}

data RxData
