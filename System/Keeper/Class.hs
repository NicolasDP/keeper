------------------------------------------------------------------------------
-- |
-- Module      : System.Keeper.Class
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unix
--
module System.Keeper.Class
    ( Keeper(..)
    ) where

------------------------------------------------------------------------------

import System.Keeper.Types

------------------------------------------------------------------------------
-- | Class Keeper
class Keeper c where
    --------------------------------------------------------------------------
    -- | Same meaning as Data.List (fold*): reducing a container 'c' with the
    -- binary opperation and a starting value 'a'
    getAuthorizedKeys :: (KKEntity -> a -> a)
                      -> a
                      -> c
                      -> a
