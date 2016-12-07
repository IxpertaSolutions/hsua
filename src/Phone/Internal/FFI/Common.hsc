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

import Foreign.C.Types (CInt)

#include <pjsua-lib/pjsua.h>

data CallSetting
data MsgData
data Reason
data SipEvent
data UserData
data MediaConfig

type CallId = CInt
type PjStatus = CInt

-- | Equivalent to PJ_TRUE
pjTrue :: CInt
pjTrue = #{const PJ_TRUE}

-- | Equivalent to PJ_FALSE
pjFalse :: CInt
pjFalse = #{const PJ_FALSE}

-- | Equivalent to PJ_SUCCESS
pjSuccess :: CInt
pjSuccess = #{const PJ_SUCCESS}

data RxData
