{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Module:       $HEADER$
-- Description:  Low level FFI.
-- Copyright:
-- License:      GPL-2
--
-- Maintainer:   Jan Sipr <jan.sipr@ixperta.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
module Phone.Account
    ( Account(..)
    , AccountId
    , AuthScheme(..)
    , WhenRegister(..)
    , createAccount
    , FFI.isAccountRegistered
    , mkSimpleAccount
    , FFI.removeAccount
    , registerAccount
    , unregisterAccount
    )
  where

import Control.Monad ((>>=))
import Data.Function (($))
import Data.Monoid ((<>))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import System.IO (IO)
import Text.Show (Show)

import Data.Text (Text)
import qualified Data.Text as T (unpack)

import Phone.Exception
    ( PhoneException
        ( CreateAccount
        , Registration
        , Unregistration
        )
    )
import Phone.Internal.FFI.Account (AccountId)
import qualified Phone.Internal.FFI.Account as FFI
    ( credDataPlainPasswd
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
import qualified Phone.Internal.FFI.Common as FFI (pjFalse, pjTrue)
import qualified Phone.Internal.FFI.PjString as FFI (withPjString)
import qualified Phone.Internal.Utils as FFI (check)


data AuthScheme = Digest | Basic
  deriving (Show)

data Account = Account
    { accountId :: Text
    -- ^ The full SIP URL for the account. The value can take name address or
    -- URL format, and will look something like "sip:account@serviceprovider"
    -- or "\"Display Name" <sip:account>"
    , registrationUri :: Text
    , realm :: Text
    , authScheme :: AuthScheme
    , userName :: Text
    , password :: Text
    }
  deriving (Show)

data WhenRegister = Now | Later

mkSimpleAccount
    :: Text -- ^ Sip server domain name or IP
    -> Text -- ^ Account name
    -> Text -- ^ Password
    -> Account
mkSimpleAccount server user password = Account
    { accountId = "sip:" <> user <> "@" <> server
    , registrationUri = "sip:" <> server
    , realm = "*"
    , authScheme = Digest
    , userName = user
    , password = password
    }

createAccount :: WhenRegister -> Account -> IO AccountId
createAccount whenReg Account{..} =
    FFI.withPjString (T.unpack accountId) $ \accountIdPjStr ->
    FFI.withPjString (T.unpack registrationUri) $ \registrationUriPjStr ->
    FFI.withPjString (T.unpack realm) $ \realmPjStr ->
    FFI.withPjString (T.unpack $ schemeText authScheme) $ \schemePjStr ->
    FFI.withPjString (T.unpack userName) $ \userNamePjStr ->
    FFI.withPjString (T.unpack password) $ \passwordPjStr ->
    FFI.withAccountConfig $ \accCfg -> do
        FFI.setAccountId accCfg accountIdPjStr
        FFI.setAccountRegUri accCfg registrationUriPjStr
        FFI.setAccountCredCount accCfg 1
        FFI.setAccountRealm accCfg 0 realmPjStr
        FFI.setAccountScheme accCfg 0 schemePjStr
        FFI.setAccountUsername accCfg 0 userNamePjStr
        FFI.setAccountDataType accCfg 0 FFI.credDataPlainPasswd
        FFI.setAccountData accCfg 0 passwordPjStr
        FFI.setAccountRegisterOnAdd accCfg $ toVal whenReg
        alloca $ \accId -> do
            FFI.setAccount accCfg FFI.pjTrue accId >>= FFI.check CreateAccount
            peek accId
  where
    toVal Now = FFI.pjTrue
    toVal Later = FFI.pjFalse

    schemeText Digest = "digest"
    schemeText Basic = "basic"

registerAccount :: AccountId -> IO ()
registerAccount accId =
    FFI.setAccountRegistration accId FFI.pjTrue >>= FFI.check Registration

unregisterAccount :: AccountId -> IO ()
unregisterAccount accId =
    FFI.setAccountRegistration accId FFI.pjFalse >>= FFI.check Unregistration

