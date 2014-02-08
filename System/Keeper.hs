module System.Keeper
    ( module System.Keeper.Database
    , module System.Keeper.Model
    , Version
    , keeperVersion
    ) where


import System.Keeper.Model
import System.Keeper.Database

data Version = Version
    { major :: Int
    , minor :: Int
    , revision :: Int
    } deriving (Eq, Ord)

instance Show Version where
    show (Version ma mi re) =
        (show ma) ++ "." ++ (show mi) ++ "." ++ (show re)

keeperVersion = Version 0 1 0
