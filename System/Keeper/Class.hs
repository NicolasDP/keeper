module System.Keeper.Class
    ( Keeper(..)
    ) where

import System.Keeper.Types

class Keeper c where
    getAuthorizedKeys :: (KKEntity -> a -> a) -> a -> c -> a
