module System.Keeper.Types where

import Data.ByteString as BS (ByteString, empty)
import Data.Byteable

data KKEntity = KKEntity
    { name              :: ByteString
    , pubKey            :: ByteString
    , certAuthority     :: Maybe Bool
    , command           :: Maybe ByteString
    , environment       :: Maybe ByteString
    , from              :: Maybe ByteString
    , noAgentForwarding :: Maybe Bool
    , noPortForwarding  :: Maybe Bool
    , noPTY             :: Maybe Bool
    , noUserRC          :: Maybe Bool
    , noX11Forwarding   :: Maybe Bool
    , permitOpen        :: Maybe ByteString
    , principals        :: Maybe ByteString
    , tunnel            :: Maybe ByteString
    } deriving (Show, Read, Eq)

defaultKKEntity :: KKEntity
defaultKKEntity = KKEntity
    { name              = BS.empty
    , pubKey            = BS.empty
    , certAuthority     = Just False
    , command           = Nothing
    , environment       = Nothing
    , from              = Nothing
    , noAgentForwarding = Just False
    , noPortForwarding  = Just False
    , noPTY             = Just False
    , noUserRC          = Just False
    , noX11Forwarding   = Just False
    , permitOpen        = Nothing
    , principals        = Nothing
    , tunnel            = Nothing
    }

newtype UserName = UserName ByteString
    deriving (Show, Read, Eq, Ord)

instance Byteable UserName where
    toBytes (UserName b) = b
