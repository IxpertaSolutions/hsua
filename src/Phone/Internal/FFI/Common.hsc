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

-- This allows to retrieve value from enums and defines
-- Pjsua uses extremely tricky enums...
#let enumToValue t = "%d", (int)t

#include <pjsua-lib/pjsua.h>

data CallSetting
data MsgData
data PjString
data Reason
data SipEvent
data UserData

type CallId = CInt
type PjStatus = CInt

-- | Equivalent to PJ_TRUE
pjTrue :: CInt
pjTrue = #{enumToValue PJ_TRUE}

-- | Equivalent to PJ_FALSE
pjFalse :: CInt
pjFalse = #{enumToValue PJ_FALSE}

-- | Equivalent to PJ_SUCCESS
pjSuccess :: CInt
pjSuccess = #{enumToValue PJ_SUCCESS}

data RxData
