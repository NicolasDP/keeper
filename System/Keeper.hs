-- |
-- Module      : System.Keeper.Database
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unix
--
-- System.Keeper is a set of tools to manage authorized ssh keys.
module System.Keeper
    ( module System.Keeper.Model
    , module System.Keeper.Database
    , Version
    , keeperVersion
    ) where

import System.Keeper.Model
import System.Keeper.Database

-- | Version field
data Version = Version
    { major :: Int    -- ^ API changes
    , minor :: Int    -- ^ API addition, no API changes
    , revision :: Int -- ^ Only bug fixing and minor updates
    } deriving (Eq, Ord)

instance Show Version where
    show (Version ma mi re) =
        (show ma) ++ "." ++ (show mi) ++ "." ++ (show re)

-- | The current software version
keeperVersion :: Version
keeperVersion = Version 0 1 0
