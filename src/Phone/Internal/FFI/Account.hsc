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
module Phone.Internal.FFI.Account
  where

-- This allows to retrieve value from enums and defines
-- Pjsua uses extremely tricky enums...
#let enumToValue t = "%d", (int)t

#include <pjsua-lib/pjsua.h>

import Foreign.C.Types (CInt(CInt))
import Foreign.Ptr (Ptr)

import System.IO (IO)

import Phone.Internal.FFI.Common (PjStatus, PjString)

type AccountId = CInt
data AccountConfig

foreign import ccall "crete_pjsua_acc_config" createAccountConfig
    :: IO (Ptr AccountConfig)

foreign import ccall "pjsua_acc_config_default" defaultAccountConfig
    :: (Ptr AccountConfig) -> IO ()

foreign import ccall "setAccountId" setAccoutId
    :: (Ptr AccountConfig) -> Ptr PjString -> IO ()

foreign import ccall "setAccountRegUri" setAccountRegUri
    :: (Ptr AccountConfig) -> Ptr PjString -> IO ()

foreign import ccall "setAccountCredCount" setAccountCredCount
    :: (Ptr AccountConfig) -> CInt -> IO ()

foreign import ccall "setAccountRealm" setAccountRealm
    :: (Ptr AccountConfig) -> CInt -> Ptr PjString -> IO ()

foreign import ccall "setAccountScheme" setAccountScheme
    :: (Ptr AccountConfig) -> CInt -> Ptr PjString -> IO ()

foreign import ccall "setAccountUsername" setAccountUsername
    :: (Ptr AccountConfig) -> CInt -> Ptr PjString -> IO ()

credDataPlainPasswd :: CInt
credDataPlainPasswd = #{enumToValue PJSIP_CRED_DATA_PLAIN_PASSWD}

foreign import ccall "setAccountDataType" setAccountDataType
    :: (Ptr AccountConfig) -> CInt -> CInt -> IO ()

foreign import ccall "setAccountData" setAccountData
    :: (Ptr AccountConfig) -> CInt -> Ptr PjString -> IO ()

-- | Add a new account to hsua (pjsua). If registration is configured for this
-- account, this function would also start the SIP registration session with
-- the SIP registrar server. This SIP registration session will be maintained
-- internally by the library, and application doesn't need to do anything to
-- maintain the registration session.
foreign import ccall "pjsua_acc_add" setAccount
    :: Ptr AccountConfig
    -- ^ Account configuration.
    -> CInt
    -- ^ If non-zero, this account will be set as the default account. The
    -- default account will be used when sending outgoing requests
    -- (e.g. making call) when no account is specified, and when receiving
    -- incoming requests when the request does not match any accounts. It is
    -- recommended that default account is set to local/LAN account.
    -> Ptr AccountId
    -- ^ Pointer to receive account ID of the new account.
    -> IO PjStatus

-- | Check if the account with given ID is registred. Boolean is simulated by
-- CInt.
--
-- Return value:
--
-- * 0 - unregistred
-- * non-zero - registred
foreign import ccall "is_accout_registred" isAccoutRegistred
    :: AccountId
    -> IO CInt
