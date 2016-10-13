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
    , createAccount
    , isAccountRegistered
    )
  where

import Control.Monad ((>>=), return)
import Data.Bool (Bool)
import Data.Eq ((/=))
import Data.Function ((.))
import Data.Text (Text, unpack)
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (malloc)
import Foreign.Storable (peek)
import System.IO (IO)

import Phone.Internal.FFI.Account
    ( AccountId
    , createAccountConfig
    , credDataPlainPasswd
    , defaultAccountConfig
    , isAccoutRegistred
    , setAccount
    , setAccountCredCount
    , setAccountData
    , setAccountDataType
    , setAccountRealm
    , setAccountRegUri
    , setAccountScheme
    , setAccountUsername
    , setAccoutId
    )
import Phone.Internal.FFI.Common (pjTrue)
import Phone.Internal.FFI.PjString (createPjString)

data AuthScheme = Digest | Basic

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

createAccount :: Account -> IO AccountId
createAccount Account{..} = do
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
    accId <- malloc
    _ <- setAccount accCfg pjTrue accId
    peek accId
  where
    newCString' = newCString . unpack
    schemeText Digest = "digest"
    schemeText Basic = "basic"

isAccountRegistered :: AccountId -> IO Bool
isAccountRegistered acc = isAccoutRegistred acc >>= (return . (/=) 0)
