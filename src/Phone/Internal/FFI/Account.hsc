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
    ( AccountId
    , credDataPlainPasswd
    , isAccountRegistered
    , removeAccount
    , setAccount
    , setAccountCredCount
    , setAccountData
    , setAccountDataType
    , setAccountId
    , setAccountRealm
    , setAccountRegUri
    , setAccountRegisterOnAdd
    , setAccountRegistration
    , setAccountScheme
    , setAccountUsername
    , withAccountConfig
    )
  where

#include <pjsua-lib/pjsua.h>

import Prelude (Num((*)))

import Control.Applicative (pure)
import Data.Bool (Bool, (&&))
import Data.Function (($), (.))
import Data.Int (Int)
import Data.Ord ((>=), (<), (>))
import Foreign.C.Types (CInt(CInt))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import System.IO (IO)

import Phone.Internal.FFI.Common (PjStatus, PjString)
import Phone.Internal.FFI.PjString (setPjString)


type AccountId = CInt
data AccountConfig
data AccountInfo
data CredInfo

withAccountConfig :: (Ptr AccountConfig -> IO a) -> IO a
withAccountConfig f = allocaBytes #{size pjsua_acc_config} $ \cfg -> do
    defaultAccountConfig cfg
    f cfg

foreign import ccall "pjsua_acc_config_default" defaultAccountConfig
    :: Ptr AccountConfig -> IO ()

setAccountId :: Ptr AccountConfig -> Ptr PjString -> IO ()
setAccountId = setPjString . #{ptr pjsua_acc_config, id}

setAccountRegUri :: Ptr AccountConfig -> Ptr PjString -> IO ()
setAccountRegUri = setPjString . #{ptr pjsua_acc_config, reg_uri}

setAccountCredCount :: Ptr AccountConfig -> CInt -> IO ()
setAccountCredCount = #{poke pjsua_acc_config, cred_count}

credInfo :: Ptr AccountConfig -> Int -> Ptr CredInfo
credInfo cfg i = #{ptr pjsua_acc_config, cred_info} cfg
    `plusPtr` (i * #{size pjsip_cred_info})

setAccountRealm :: Ptr AccountConfig -> Int -> Ptr PjString -> IO ()
setAccountRealm cfg =
    setPjString . #{ptr pjsip_cred_info, realm} . credInfo cfg

setAccountScheme :: Ptr AccountConfig -> Int -> Ptr PjString -> IO ()
setAccountScheme cfg =
    setPjString . #{ptr pjsip_cred_info, scheme} . credInfo cfg

setAccountUsername :: Ptr AccountConfig -> Int -> Ptr PjString -> IO ()
setAccountUsername cfg =
    setPjString . #{ptr pjsip_cred_info, username} . credInfo cfg

credDataPlainPasswd :: CInt
credDataPlainPasswd = #{const PJSIP_CRED_DATA_PLAIN_PASSWD}

setAccountDataType :: Ptr AccountConfig -> Int -> CInt -> IO ()
setAccountDataType cfg = #{poke pjsip_cred_info, data_type} . credInfo cfg

setAccountData :: Ptr AccountConfig -> Int -> Ptr PjString -> IO ()
setAccountData cfg =
    setPjString . #{ptr pjsip_cred_info, data} . credInfo cfg

setAccountRegisterOnAdd :: Ptr AccountConfig -> CInt -> IO ()
setAccountRegisterOnAdd = #{poke pjsua_acc_config, register_on_acc_add}

-- | Update registration or perform unregistration. If registration is
-- configured for this account, then initial SIP REGISTER will be sent when the
-- account is added with setAccount. Application normally only need to
-- call this function if it wants to manually update the registration or to
-- unregister from the server.
--
-- If given _false_ the unregistration will be performed.
foreign import ccall "pjsua_acc_set_registration" setAccountRegistration
    :: AccountId
    -> CInt
    -- ^ pj_bool_t --> _zero_ = false, _non-zero_ = _true_
    -> IO PjStatus


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

-- | This will unregister the account from the SIP server, if necessary, and
-- terminate server side presence subscriptions associated with this account.
foreign import ccall "pjsua_acc_del" removeAccount
    :: AccountId
    -> IO PjStatus

foreign import ccall "pjsua_acc_get_info" getAccountInfo
    :: AccountId -> Ptr AccountInfo -> IO ()

withAccountInfo :: (Ptr AccountInfo -> IO a) -> IO a
withAccountInfo = allocaBytes #{size pjsua_acc_info}

isAccountRegistered :: AccountId -> IO Bool
isAccountRegistered a = withAccountInfo $ \info -> do
    getAccountInfo a info
    status <- #{peek pjsua_acc_info, status} info
    expires <- #{peek pjsua_acc_info, expires} info
    pure $ (status :: CInt) >= 200 && status < 300 && (expires :: CInt) > 0
