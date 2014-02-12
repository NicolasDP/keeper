{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
import System.Keeper

import Data.Aeson
import Data.Aeson.Types

import Control.Applicative
import Control.Monad
import Data.List.Split as S (splitOn)

import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BL

instance FromJSON BS.ByteString where
    parseJSON (String t) = pure . encodeUtf8 $ t
    parseJSON v          = typeMismatch "ByteString" v
    {-# INLINE parseJSON #-}

instance FromJSON KKEntity where
    parseJSON (Object v) =
        KKEntity <$> v .:  "name"
                 <*> v .:  "pubKey"
                 <*> v .:? "certAuthority"
                 <*> v .:? "command"
                 <*> v .:? "environment"
                 <*> v .:? "from"
                 <*> v .:? "noAgentForwarding"
                 <*> v .:? "noPortForwarding"
                 <*> v .:? "noPTY"
                 <*> v .:? "noUserRC"
                 <*> v .:? "noX11Forwarding"
                 <*> v .:? "permitOpen"
                 <*> v .:? "principals"
                 <*> v .:? "tunnel"
    parseJSON _ = mzero

instance ToJSON KKEntity where
    toJSON (k) =
        object [ "name" .= (show $ name k)
               , "pubKey" .= (show $ pubKey k)
               , "certAuthority" .= (certAuthority k)
               , "command" .= (show $ command k)
               , "environment" .= (show $ environment k)
               , "from" .= (show $ from k)
               , "noAgentForwarding" .= (noAgentForwarding k)
               , "noPortForwarding" .= (noPortForwarding k)
               , "noPTY" .= (noPTY k)
               , "noUserRC" .= (noUserRC k)
               , "noX11Forwarding" .= (noX11Forwarding k)
               , "permitOpen" .= (show $ permitOpen k)
               , "principals" .= (show $ principals k)
               , "tunnel" .= (show $ tunnel k)
                 ]

getHomePath :: String -> IO String
getHomePath username = do
    fileContent <- Prelude.readFile "/etc/passwd"
    let contentLines = lines fileContent
    let result = Prelude.drop 5 $ getUser username $ Prelude.map (S.splitOn ":") contentLines
    if Prelude.null result then error $ "no database for user: " ++ username
                           else return $ Prelude.head result
    where
        getUser _ [] = []
        getUser n [(user:xs)] = if user == n then (user:xs) else []
        getUser n ((user:xs):xss) = if user == n then (user:xs) else getUser n xss
        getUser n (_:xss) = getUser n xss

instance Keeper [KKEntity] where
    getAuthorizedKeys c f = mapM_ f c

getJSON :: String -> IO BL.ByteString
getJSON fpath = BL.readFile fpath

getDB :: UserName -> IO [KKEntity]
getDB (UserName userName) = do
    homePath <- getHomePath (BS.unpack userName)
    content <- getJSON $ homePath ++ "/.ssh/authorized_keys.keeper"
    let d = eitherDecode content :: Either String [KKEntity]
    case d of
        Left err -> error err
        Right xs -> return xs

--main = BL.writeFile "test.txt" $ encode defaultKKEntity
main = defaultMain getDB
