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
    , isAccountRegistered
    , mkSimpleAccount
    , registerAccount
    , removeAccount
    , unregisterAccount
    )
  where

import Data.Bool (Bool)
import Data.Function (($), (.))
import Data.Monoid ((<>))
import Text.Show (Show)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT(ContT), evalContT)
import Data.Text (Text)

import Phone.Exception
    ( PhoneException
        ( CreateAccount
        , Registration
        , RemoveAccount
        , Unregistration
        )
    )
import Phone.MonadPJ (MonadPJ(liftPJ))
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
import qualified Phone.Internal.Utils as FFI (check, checkPeek)


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

createAccount :: MonadPJ m => WhenRegister -> Account -> m AccountId
createAccount whenReg Account{..} = liftPJ . evalContT $ do
    accountIdPjStr <- ContT $ FFI.withPjString accountId
    registrationUriPjStr <- ContT $ FFI.withPjString registrationUri
    realmPjStr <- ContT $ FFI.withPjString realm
    schemePjStr <- ContT $ FFI.withPjString (schemeText authScheme)
    userNamePjStr <- ContT $ FFI.withPjString userName
    passwordPjStr <- ContT $ FFI.withPjString password
    accCfg <- ContT FFI.withAccountConfig
    lift $ do
        FFI.setAccountId accCfg accountIdPjStr
        FFI.setAccountRegUri accCfg registrationUriPjStr
        FFI.setAccountCredCount accCfg 1
        FFI.setAccountRealm accCfg 0 realmPjStr
        FFI.setAccountScheme accCfg 0 schemePjStr
        FFI.setAccountUsername accCfg 0 userNamePjStr
        FFI.setAccountDataType accCfg 0 FFI.credDataPlainPasswd
        FFI.setAccountData accCfg 0 passwordPjStr
        FFI.setAccountRegisterOnAdd accCfg $ toVal whenReg
        FFI.checkPeek CreateAccount $ FFI.setAccount accCfg FFI.pjTrue
  where
    toVal Now = FFI.pjTrue
    toVal Later = FFI.pjFalse

    schemeText :: AuthScheme -> Text
    schemeText Digest = "digest"
    schemeText Basic = "basic"

registerAccount :: MonadPJ m => AccountId -> m ()
registerAccount accId = liftPJ . FFI.check Registration
    $ FFI.setAccountRegistration accId FFI.pjTrue

unregisterAccount :: MonadPJ m => AccountId -> m ()
unregisterAccount accId = liftPJ . FFI.check Unregistration
    $ FFI.setAccountRegistration accId FFI.pjFalse

isAccountRegistered :: MonadPJ m => AccountId -> m Bool
isAccountRegistered = liftPJ . FFI.isAccountRegistered

removeAccount :: MonadPJ m => AccountId -> m ()
removeAccount accId = liftPJ . FFI.check RemoveAccount
    $ FFI.removeAccount accId
