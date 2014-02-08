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
-- authorized keys for different user.
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

-- | Insert a new user in the given base
-- The following command insert a new key for user *vincent* in the base *nicolas* and
-- ask ssh-daemon to execute the command *bunker_checkruserright vincent*
--
-- > insertHitUser "nicolas" <vincent's key> "vincent" "bunker_checkuserright vincent"
insertKeeperKey base name key ca cmd env from naf npf npty nurc nx11 po pri tunnel =
    runDB base $
        insertBy $
            KeeperKey name key ca            -- certAuthority
                      cmd env from           -- command environment from
                      naf npf npty nurc nx11 -- noAgentForwarding noPortForwarding noURC nX11
                      po pri tunnel          -- permitOpen principals tunnel

-- | Get the list of authorized keys for the given base
--
-- > selectAuthorizedKeys "nicolas"
selectAuthorizedKeys base =
    runDB base $ selectList [] []

-- | Get the list of authorized keys for the given base
--
-- > selectAuthorizedKeys "nicolas"
selectAuthorizedKeysOf base name =
    runDB base $ selectList [KeeperKeyName ==. name] []

-- | delete all user's keys in the given base
--
-- > deleteHitUser "nicolas" "vincent"
deleteKeeperKeys base name =
    runDB base $ deleteWhere $ [KeeperKeyName ==. name]

-- | delete a user's key
--
-- > deleteHitUserKey "nicolas" (PersistInt64 19)
deleteKeeperKey base kId =
    runDB base $ deleteWhere $ [KeeperKeyId ==. kId]
