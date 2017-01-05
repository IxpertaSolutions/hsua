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
module Phone.Event
    ( Event(..)
    )
  where

import Phone.Internal.Event
    ( Event
        ( Unknown
        , Timer
        , TxMsg
        , RxMsg
        , TransportError
        , TransactionState
        , User
        )
    )
