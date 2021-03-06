{-# LANGUAGE DeriveAnyClass    #-}
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
module Phone.Exception
    ( PhoneException(..)
    )
  where

import Control.Exception.Base (Exception)
import Data.Typeable (Typeable)
import Text.Show (Show)

data PhoneException
    = AnswerCall
    | CreateLib
    | HangupCall
    | Initialization
    | MakeCall
    | Start
    | Transport
    | GetCallInfo
    | Registration
    | Unregistration
    | CreateAccount
    | RemoveAccount
    | UnknowenEventType
  deriving (Typeable, Show, Exception)
