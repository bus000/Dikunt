{-# LANGUAGE DeriveDataTypeable #-}
module BotTypes
    ( Bot(..)
    , bot
    , IRCMessage(..)
    , IRCCommand(..)
    , IRCPrefix(..)
    , IRCUser(..)
    , ServerMessage(..)
    , ClientMessage(..)
    , nicknamePrefix

    -- Bot configuration declaration.
    , BotConfig(..)

    -- Type declarations.
    , Nickname
    , Channel
    , Password
    , Servername
    , Username
    , Hostname
    , Mode
    , Realname
    ) where

import Control.Concurrent.MVar (MVar)
import System.IO (Handle)
import Monitoring (Monitor)

type Nickname = String
type Channel = String
type Password = String
type Servername = String
type Username = String
type Hostname = String
type Mode = Integer
type Realname = String

data BotConfig = BotConfig
    { configServer     :: Servername
    , configNickname   :: Nickname
    , configPassword   :: Password
    , configChannel    :: Channel
    , configPort       :: Integer
    } deriving (Show, Eq)

{- | The IRC bot parameters. -}
data Bot = Bot
    { socket        :: !Handle -- ^ The handle to communicate with the server.
    , nickname      :: !Nickname -- ^ The nickname used on the server.
    , channel       :: !Channel -- ^ The channel connected to.
    , password      :: !Password -- ^ The password to connect with.
    , pluginMonitor :: MVar Monitor -- ^ Plugin in- and out- put.
    }

{- | Represents an IRC command. -}
data IRCCommand = ADMIN | AWAY | CNOTICE | CPRIVMSG | CONNECT | DIE | ENCAP
    | ERROR | HELP | INFO | INVITE | ISON | JOIN | KICK | KILL | KNOCK | LINKS
    | LIST | LUSERS | MODE | MOTD | NAMES | NAMESX | NICK | NOTICE | OPER | PART
    | PASS | PING | PONG | PRIVMSG | QUIT | REHASH | RESTART | RULES | SERVER
    | SERVICE | SERVLIST | SQUERY | SQUIT | SETNAME | SILENCE | STATS | SUMMON
    | TIME | TOPIC | TRACE | UHNAMES | USER | USERHOST | USERIP | USERS
    | VERSION | WALLOPS | WATCH | WHO | WHOIS | WHOWAS | NUMCOM Integer
    deriving (Show, Read, Eq)

{- | Represent an IRC message of any type which is generally structured as
 - ":<prefix> <command> <params> :<trailing>\r\n". The prefix and trail might
 - not exist and is therefore of a Maybe type. -}
data IRCMessage = IRCMessage
    { _ircPrefix  :: Maybe IRCPrefix -- ^ The prefix of an IRC message.
    , _ircCommand :: IRCCommand -- ^ The command of the message.
    , _ircParams  :: [String] -- ^ List of parameters.
    , _trail      :: Maybe String -- ^ Actual message.
    } deriving (Show, Eq)

{- | An IRC prefix consist either of a server name or a user nick with an
 - optional user name and host name. This type represent the IRC prefix. -}
data IRCPrefix = ServernamePrefix Servername
    | NicknamePrefix IRCUser
    deriving (Show, Read, Eq)

data IRCUser = IRCUser Nickname (Maybe Username) (Maybe Hostname)
    deriving (Show, Read, Eq)

data ServerMessage = ServerNick IRCUser Nickname
    | ServerJoin IRCUser Channel
    | ServerPart IRCUser Channel String
    | ServerQuit IRCUser String
    | ServerTopic IRCUser Channel String
    | ServerInvite IRCUser Nickname Channel
    | ServerPrivMsg IRCUser Nickname String
    | ServerNotice IRCUser Nickname String
    | ServerPing Servername
    | ServerReply Servername Integer [String] (Maybe String)
    deriving (Eq, Show, Read)

data ClientMessage = ClientPass Password
    | ClientNick Nickname
    | ClientUser Username Mode Realname
    | ClientOper Username Password
    | ClientMode Nickname String
    | ClientQuit String -- Reason for leaving.
    | ClientJoin [(String, String)]
    | ClientPart [Channel] (Maybe String)
    | ClientTopic Channel (Maybe String)
    | ClientNames [Channel]
    | ClientList [Channel]
    | ClientInvite Nickname Channel
    | ClientPrivMsg IRCUser String
    | ClientPrivMsgChan Channel String
    | ClientNotice IRCUser String
    | ClientWho String
    | ClientWhoIs (Maybe Servername) Username
    | ClientWhoWas Username (Maybe Integer) (Maybe Servername)
    | ClientPing Servername
    | ClientPong Servername
    deriving (Eq, Show, Read)

{- | Construct a Bot to handle a IRC chat. -}
bot :: Handle
    -- ^ Socket to an IRC server.
    -> String
    -- ^ Nickname to use on the server.
    -> String
    -- ^ Channel to connect to, should start with a #.
    -> String
    -- ^ Password to use.
    -> MVar Monitor
    -- ^ Input and output file handles for plugins.
    -> Bot
bot = Bot

{- | Construct a IRCPrefix representing a NicknamePrefix of an IRCUser. -}
nicknamePrefix :: Nickname
    -- ^ Nick of the prefix.
    -> Maybe Username
    -- ^ Maybe username of the user.
    -> Maybe String
    -- ^ Maybe hostname of the user.
    -> IRCPrefix
nicknamePrefix nick Nothing Nothing =
    NicknamePrefix $ IRCUser nick Nothing Nothing
nicknamePrefix nick user Nothing =
    NicknamePrefix $ IRCUser nick user Nothing
nicknamePrefix nick Nothing host =
    NicknamePrefix $ IRCUser nick Nothing host
nicknamePrefix nick user host =
    NicknamePrefix $ IRCUser nick user host
