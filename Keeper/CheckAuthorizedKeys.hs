{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
------------------------------------------------------------------------------
-- |
-- Module      : Keeper.CheckAuthorizedKeys
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unix

------------------------------------------------------------------------------

import System.Keeper
import System.Keeper.Backend.Dummy

------------------------------------------------------------------------------

main = defaultMain getFlatDB
