module System.Keeper.Types
    ( KKEntity(..)
    , defaultKKEntity
    , UserName(..)
    ) where

import Data.ByteString as BS (ByteString, empty)
import Data.Byteable

-- | Keeper Key Entity:
-- it represents an AUTHORIZED_KEY. It implements the FORMAT given in the
-- SSHD_CONFIG manual.
data KKEntity = KKEntity
    { name              :: ByteString       -- ^ an associated name
    , pubKey            :: ByteString       -- ^ the public KEY
    , certAuthority     :: Maybe Bool       -- ^ cert-authority
    , command           :: Maybe ByteString -- ^ command="command"
    , environment       :: Maybe ByteString -- ^ environment="NAME=value"
    , from              :: Maybe ByteString -- ^ from="pattern-list"
    , noAgentForwarding :: Maybe Bool       -- ^ no-agent-forwarding
    , noPortForwarding  :: Maybe Bool       -- ^ no-port-forwarding
    , noPTY             :: Maybe Bool       -- ^ no-pty
    , noUserRC          :: Maybe Bool       -- ^ no-user-rc
    , noX11Forwarding   :: Maybe Bool       -- ^ no-X11-forwarding
    , permitOpen        :: Maybe ByteString -- ^ permitopen="host:port"
    , principals        :: Maybe ByteString -- ^ principals="principals"
    , tunnel            :: Maybe ByteString -- ^ tunnel="n"
    } deriving (Show, Read, Eq)

-- | returns a default KKEntity
defaultKKEntity :: KKEntity
defaultKKEntity = KKEntity
    { name              = BS.empty
    , pubKey            = BS.empty
    , certAuthority     = Nothing
    , command           = Nothing
    , environment       = Nothing
    , from              = Nothing
    , noAgentForwarding = Nothing
    , noPortForwarding  = Nothing
    , noPTY             = Nothing
    , noUserRC          = Nothing
    , noX11Forwarding   = Nothing
    , permitOpen        = Nothing
    , principals        = Nothing
    , tunnel            = Nothing
    }

newtype UserName = UserName ByteString
    deriving (Show, Read, Eq, Ord)

instance Byteable UserName where
    toBytes (UserName b) = b
