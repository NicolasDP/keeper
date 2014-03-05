-- |
-- Module      : System.GitControl
-- License     : BSD-style
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE OverloadedStrings #-}
module System.Keeper
    ( defaultMain
    , module System.Keeper.Class
    , module System.Keeper.Types
    ) where

import System.Posix.Env.ByteString
import System.Posix.Process.ByteString
import System.Exit

import System.Keeper.Class
import System.Keeper.Types

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (intersperse)
import Data.Byteable

defaultMain :: Keeper a
            => (UserName -> IO a) -- ^ initialize the keeper backend
            -> IO ()
defaultMain getBackend = do
    args <- getArgs
    case args of
        [user] -> doCheck $ UserName user
        _      -> error "invalid command line"
  where
        doCheck user = do
            backend <- getBackend user
            let list = getAuthorizedKeys showKey [] backend
            mapM_ BS.putStrLn list

        showKey :: KKEntity -> [BS.ByteString]-> [BS.ByteString]
        showKey k acc = let certa = maybe ([]   ) (\c -> if c then ["cert-authority"] else []) $ certAuthority k
                            comma = maybe (certa) (\c -> (BS.concat ["command=\""    ,c,"\""]):certa) $ command k
                            envir = maybe (comma) (\c -> (BS.concat ["environment=\"",c,"\""]):comma) $ environment k
                            fro   = maybe (envir) (\c -> (BS.concat ["from=\""       ,c,"\""]):envir) $ from k
                            noage = maybe (fro  ) (\c -> if c then ("no-agent-forwarding"):fro else fro) $ noAgentForwarding k
                            nopor = maybe (noage) (\c -> if c then ("no-port-forwarding"):noage else noage) $ noPortForwarding k
                            nopty = maybe (nopor) (\c -> if c then ("no-pty"):nopor else nopor) $ noPTY k
                            nouse = maybe (nopty) (\c -> if c then ("no-user-rc"):nopty else nopty) $ noUserRC k
                            nox11 = maybe (nouse) (\c -> if c then ("no-x11-forwarding"):nouse else nouse) $ noX11Forwarding k
                            permi = maybe (nox11) (\c -> (BS.concat ["permitopen=\"",c,"\""]):nox11) $ permitOpen k
                            princ = maybe (permi) (\c -> (BS.concat ["principals=\"",c,"\""]):permi) $ principals k
                            tunne = maybe (princ) (\c -> (BS.concat ["tunnels=\""   ,c,"\""]):princ) $ tunnel k
                            options = BS.intercalate "," tunne
                            outPutKey = BS.concat [pubKey k," ",options, " # ",name k]
                        in outPutKey:acc
