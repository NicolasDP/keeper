{-# LANGUAGE OverloadedStrings #-}
import System.Keeper
import Database.Persist

import Data.ByteString.Char8 as BS

import System.Posix.Env.ByteString
import qualified Data.String (lines)
import qualified Data.List.Split as S (splitOn)

import System.Log.Logger
import System.Log.Handler.Syslog

getHomePath dbname = do
    let userName = unpack dbname
    fileContent <- Prelude.readFile "/etc/passwd"
    let contentLines = Data.String.lines fileContent
    let result = Prelude.drop 5 $ getUser userName $ Prelude.map (S.splitOn ":") contentLines
    if Prelude.null result then error $ "no database for user: " ++ userName
                           else return $ pack $ Prelude.head result
    where
        getUser _ [] = []
        getUser n [(user:xs)] = if user == n then (user:xs) else []
        getUser n ((user:xs):xss) = if user == n then (user:xs) else getUser n xss
        getUser n (_:xss) = getUser n xss

showKeysFrom dbname = do
    listRes <- selectAuthorizedKeys dbname
    Prelude.mapM_ showKeyOf listRes
    where
        showKeyOf ent =
            let val = entityVal ent
                cmd = case keeperKeyCommand val of
                          Nothing -> BS.empty
                          Just command -> BS.concat [" command=\"",command,"\""]
            in  do warningM (BS.unpack dbname) ("#name(" ++ (BS.unpack $ keeperKeyName val) ++ ")")
                   BS.putStrLn $ BS.concat [keeperKeyPubKey val,cmd]

main = do
    s <- openlog "CheckAuthorizedKey" [PID] USER WARNING
    updateGlobalLogger rootLoggerName (addHandler s)
    args <- getArgs
    case args of
        [dbname] -> do warningM "check pubkey" ("#" ++ (unpack dbname))
                       home <- getHomePath dbname
                       showKeysFrom home
        _        -> errorM "Bad command line" (show args)
