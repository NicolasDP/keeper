{-# LANGUAGE OverloadedStrings #-}
import System.Keeper
import Database.Persist

import Data.ByteString.Char8 as BS

import System.Posix.Env.ByteString

import System.Log.Logger
import System.Log.Handler.Syslog

showKeysFrom dbname = do
    listRes <- selectAuthorizedKeys (BS.concat ["/home/",dbname])
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
                       showKeysFrom dbname
        _        -> errorM "Bad command line" (show args)
