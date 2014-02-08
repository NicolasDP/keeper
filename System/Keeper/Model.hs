{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : System.Keeper.Model
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
module System.Keeper.Model where

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.Text             as T (pack)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Typeable (Typeable)

-- | The bunker's database model
share [mkPersist sqlSettings, mkMigrate "migrateTables"]
      [persistLowerCase|
KeeperKey
    name              BS.ByteString
    pubKey            BS.ByteString
    certAuthority     Bool                default=False
    command           BS.ByteString Maybe default=Nothing
    environment       BS.ByteString Maybe default=Nothing
    from              BS.ByteString Maybe default=Nothing
    noAgentForwarding Bool                default=False
    noPortForwarding  Bool                default=False
    noPTY             Bool                default=False
    noUserRC          Bool                default=False
    noX11Forwarding   Bool                default=False
    permitOpen        BS.ByteString Maybe default=Nothing
    principals        BS.ByteString Maybe default=Nothing
    tunnel            BS.ByteString Maybe default=Nothing
    UniqueName pubKey
    deriving Show Typeable
|]

-- | the default way to access the database
runDB base f =
    runSqlite (T.pack ((BS.unpack base) ++ "/.ssh/authorized_keys.keeper")) f

-- | create a new bunker database (or update the existing one) with the given
-- name
--
-- > createKeeperTable "/nicolas/nicolas"
createKeeperTable base = runDB base $ runMigration migrateTables