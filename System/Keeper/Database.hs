{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : System.Keeper.Database
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unix
--
-- Bunker's database common interface.
--
-- The idea is to create a database per login. So you can manage different
-- authorized keys for different users.
--
module System.Keeper.Database
    ( -- * Usual functions
      insertKeeperKey
    , deleteKeeperKeys
    , deleteKeeperKey
    , selectAuthorizedKeys
    , selectAuthorizedKeysOf
    ) where

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.Text             as T (pack)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Typeable (Typeable)
import qualified Database.Esqueleto as E

import System.Keeper.Model

-- | Insert a new key in the base, associated it a name (an alias)
--
-- > insertKeeperKey "/home/git" "nicolas" <mykeys> False
-- >                             Nothing Nothing Nothing
-- >                             False False False False False
-- >                             Nothing Nothing Nothing
insertKeeperKey base name key ca cmd env from naf npf npty nurc nx11 po pri tunnel =
    runDB base $
        insertBy $
            KeeperKey name key ca            -- certAuthority
                      cmd env from           -- command environment from
                      naf npf npty nurc nx11 -- noAgentForwarding noPortForwarding noURC nX11
                      po pri tunnel          -- permitOpen principals tunnel

-- | Get the list of authorized keys for the given base
--
-- > selectAuthorizedKeys "/home/git"
selectAuthorizedKeys base =
    runDB base $ selectList [] []

-- | Get the list of authorized keys for the given base associated to a given
-- name.
--
-- > selectAuthorizedKeys "/home/git" "nicolas"
selectAuthorizedKeysOf base name =
    runDB base $ selectList [KeeperKeyName ==. name] []

-- | delete all keys associated to a given name in the given base
--
-- > deleteKeeperKeys "/home/git" "nicolas"
deleteKeeperKeys base name =
    runDB base $ deleteWhere $ [KeeperKeyName ==. name]

-- | delete a specific key
--
-- > deleteKeeperKey "/home/git" ((Key $ toPersistValue 3))
deleteKeeperKey base kId =
    runDB base $ deleteWhere $ [KeeperKeyId ==. kId]
