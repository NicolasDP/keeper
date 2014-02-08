{-# LANGUAGE OverloadedStrings #-}

import System.Keeper
import Database.Persist
import Data.Int

import qualified Data.ByteString.Char8 as BS
import Options.Applicative
import System.Posix.Env.ByteString

data Command = CmdCreate
             | CmdAdd    String String
             | CmdClear  String
             | CmdDel    String
             | CmdShow   (Maybe String)

cmdCreate :: Parser Command
cmdCreate = pure CmdCreate

cmdAdd :: Parser Command
cmdAdd = CmdAdd <$> (argument str (metavar "name")) <*> (argument str (metavar "key"))

cmdClear :: Parser Command
cmdClear = CmdClear <$> (argument str (metavar "name"))

cmdDel :: Parser Command
cmdDel = CmdDel <$> (argument str (metavar "identifier"))

cmdShow :: Parser Command
cmdShow = CmdShow <$> optional (argument str (metavar "name"))

sample :: Parser Command
sample = subparser
    ( command "init" (info cmdCreate
        ( progDesc "initialize the authorized keys base"))
   <> command "add" (info cmdAdd
        ( progDesc "add a new entry"))
   <> command "clear" (info cmdClear
        ( progDesc "delete all entries"))
   <> command "del" (info cmdDel
        ( progDesc "delete a specific entry"))
   <> command "show" (info cmdShow
        ( progDesc "show the content of the authorized keys base"))
   )

version :: Parser (a -> a)
version = infoOption ("Keeper, version: " ++ (show keeperVersion))
    ( long "version"
   <> short 'v'
   <> help "print current Keeper's version")

doInsertionKey home name key = do
    entEither <- insertKeeperKey home name key
                     False Nothing Nothing Nothing
                     False False False False False
                     Nothing Nothing Nothing
    case entEither of
        (Right _) -> putStrLn "key added"
        (Left  _) -> error "key already presents in the database"

doListKeys home = do
    listRes <- selectAuthorizedKeys home
    Prelude.mapM_ showKeyOf listRes
    where
        showKeyOf ent =
            let val = entityVal ent
                keyId  = unKey $ entityKey ent
            in  putStrLn $ "id(" ++ (show keyId) ++ ") "
                        ++ "user(" ++ (BS.unpack $ keeperKeyName val) ++ ") "
                        ++ "key(" ++ (BS.unpack $ keeperKeyPubKey val) ++ ")"

doListKeysOf home name = do
    listRes <- selectAuthorizedKeysOf home name
    Prelude.mapM_ showKeyOf listRes
    where
        showKeyOf ent =
            let val = entityVal ent
                keyId  = unKey $ entityKey ent
            in  putStrLn $ "id(" ++ (show keyId) ++ ") "
                        ++ "user(" ++ (BS.unpack $ keeperKeyName val) ++ ") "
                        ++ "key(" ++ (BS.unpack $ keeperKeyPubKey val) ++ ")"

dispatchOptions :: BS.ByteString -> Command -> IO ()
dispatchOptions home (CmdCreate)           = createKeeperTable home
dispatchOptions home (CmdAdd   name key)   = doInsertionKey home (BS.pack name) (BS.pack key)
dispatchOptions home (CmdClear name)       = deleteKeeperKeys home (BS.pack name)
dispatchOptions home (CmdDel   kid)        = deleteKeeperKey home (Key $ toPersistValue (read kid :: Int64))
dispatchOptions home (CmdShow  Nothing)    = doListKeys home
dispatchOptions home (CmdShow (Just name)) = doListKeysOf home (BS.pack name)

main = do
    env <- getEnv "HOME"
    case env of
        Nothing   -> error "can't find environment variable HOME"
        Just home ->  execParser opts >>= dispatchOptions home
    where
      opts = info (helper <*> version <*> sample)
        ( fullDesc
       <> progDesc "This is the default Command Line Interface for Keeper"
       <> header ("Command Line Interface for Keeper\n" ++
                  "Keeper is a SSH Keys Database manager" ))
