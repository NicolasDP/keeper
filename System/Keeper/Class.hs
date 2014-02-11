module System.Keeper.Class where

import System.Keeper.Types

class Keeper c where
    getAuthorizedKeys :: c -> (KKEntity -> IO a) -> IO ()
