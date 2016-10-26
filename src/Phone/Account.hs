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
    , removeAccount
    , registerAccount
    , unregisterAccount
    )
  where

import Control.Applicative ((<*))
import Control.Monad ((>>=), return)
import Data.Bool (Bool)
import Data.Eq ((/=))
import Data.Function (($), (.))
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Storable (peek)
import System.IO (IO)
import Text.Show (Show)

import Phone.Exception
    ( PhoneException
        ( CreateAccount
        , Registration
        , Unregistration
        )
    )
import Phone.Internal.FFI.Account
    ( AccountId
    , createAccountConfig
    , credDataPlainPasswd
    , defaultAccountConfig
    , isAccoutRegistred
    , removeAccount
    , setAccount
    , setAccountCredCount
    , setAccountData
    , setAccountDataType
    , setAccountRealm
    , setAccountRegUri
    , setAccountRegisterOnAdd
    , setAccountRegistration
    , setAccountScheme
    , setAccountUsername
    , setAccoutId
    )
import Phone.Internal.FFI.Common (pjFalse, pjTrue)
import Phone.Internal.FFI.PjString (createPjString)
import Phone.Internal.Utils (check)


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
createAccount whenReg Account{..} = do
    accCfg <- createAccountConfig
    defaultAccountConfig accCfg
    newCString' accountId >>= createPjString >>= setAccoutId accCfg
    newCString' registrationUri >>= createPjString >>= setAccountRegUri accCfg
    setAccountCredCount accCfg 1
    newCString' realm >>= createPjString >>= setAccountRealm accCfg 0
    newCString' (schemeText authScheme) >>= createPjString
        >>= setAccountScheme accCfg 0
    newCString' userName >>= createPjString >>= setAccountUsername accCfg 0
    setAccountDataType accCfg 0 credDataPlainPasswd
    newCString' password >>= createPjString >>= setAccountData accCfg 0
    setAccountRegisterOnAdd accCfg $ toVal whenReg
    accId <- malloc
    setAccount accCfg pjTrue accId >>= check CreateAccount
    peek accId <* free accId <* free accCfg
  where
    newCString' = newCString . unpack

    toVal Now = pjTrue
    toVal Later = pjFalse

    schemeText Digest = "digest"
    schemeText Basic = "basic"

isAccountRegistered :: AccountId -> IO Bool
isAccountRegistered acc = isAccoutRegistred acc >>= (return . (/=) 0)

registerAccount :: AccountId -> IO ()
registerAccount accId =
    setAccountRegistration accId pjTrue >>= check Registration

unregisterAccount :: AccountId -> IO ()
unregisterAccount accId =
    setAccountRegistration accId pjFalse >>= check Unregistration

