{- |
 - Module      : BotTypes
 - Description : Types and instances representing a bot.
 - Copyright   : (c) Magnus Stavngaard, 2016
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - The main type is the Bot type. The bot represents a connection to a IRC
 - server on a specific channel. The bot also contains a monitor that keeps
 - track of plugin processes. The output and input of plugins can also be found
 - in the monitor. The module also defines types for messages received by the
 - server and messages that can be send to the server. The types are
 - ServerMessage and ClientMessage respectively. Both types can be converted to
 - and from JSON.
 -}
{-# LANGUAGE OverloadedStrings #-}
module BotTypes
    ( Bot(..)
    , bot
    , IRCCommand(..)
    , IRCUser(..)
    , UserServer(..)
    , ServerMessage(..)
    , ClientMessage(..)
    , Target(..)
    , getServerCommand
    , getClientCommand

    -- Bot configuration declaration.
    , BotConfig(..)

    -- Nickname type, smart constructor and getter.
    , Nickname
    , nickname
    , getNickname
    -- Hardcoded nicknames.
    , nickservNickname

    , Channel
    , Password
    , Servername
    , Username
    , Hostname
    , Mode
    , Realname
    ) where

import Control.Concurrent.MVar (MVar)
import Data.Aeson (ToJSON(..), FromJSON(..), withObject, (.=), (.:), (.:?), object, withText)
import qualified Data.Aeson.Types as Aeson
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Monitoring (DikuntMonitor)
import System.IO (Handle)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)
import Test.QuickCheck.Gen (oneof, suchThat)
import Text.Regex.PCRE ((=~))
import Utils (shrink1, shrink2, shrink3)

{- | IRC nickname that starts with a letter and after that is followed by string
 - of letters, numbers and any of the symbols [-, [, ], \, `, ^, {, }]. -}
newtype Nickname = Nickname String deriving (Show, Eq, Read)

{- | Smart constructor for Nicknames, only allow correct IRC nicknames to be
 - constructed. -}
nickname :: String -> Maybe Nickname
nickname nick
    | nick =~ nicknameRegex = Just . Nickname $ nick
    | otherwise = Nothing
  where
    nicknameRegex = "^[A-z]([0-9A-z\\-\\[\\]\\\\\\`\\^\\{\\}])*$" :: String

{- | Get the actual nickname from the nickname type. -}
getNickname :: Nickname
    -- ^ The Nickname to get nickname from.
    -> String
getNickname (Nickname nick) = nick

{- | Hardcoded nickname for the authentication server on freenode. -}
nickservNickname :: Nickname
nickservNickname = Nickname "NickServ"

{- | Construct arbitrary IRC nicknames. -}
instance Arbitrary Nickname where
    arbitrary = Nickname <$> suchThat arbitrary (isJust . nickname)

    shrink (Nickname nick) = [Nickname nick' | nick' <- shrink nick]

instance ToJSON Nickname where
    toJSON (Nickname nick) = Aeson.String . T.pack $ nick

instance FromJSON Nickname where
    parseJSON = withText "nickname" $ return . Nickname . T.unpack

{- | IRC channel. -}
type Channel = String

{- | IRC password. -}
type Password = String

{- | IRC servername. -}
type Servername = String

{- | IRC username. -}
type Username = String

{- | IRC hostname. -}
type Hostname = String

{- | IRC mode. -}
type Mode = Integer

{- | IRC real name. -}
type Realname = String

{- | Configuration used to create an IRC bot. -}
data BotConfig = BotConfig
    { configServer   :: Servername -- ^ URL of server to connect to.
    , configNickname :: Nickname -- ^ Nickname of the bot.
    , configPassword :: Password -- ^ Password of the bot.
    , configChannel  :: Channel -- ^ Channel to connect to.
    , configPort     :: Integer -- ^ Port to use when connecting to server.
    } deriving (Show, Eq)

{- | The IRC bot parameters. -}
data Bot = Bot
    { socket        :: !Handle -- ^ The handle to communicate with the server.
    , botNickname   :: !Nickname -- ^ The nickname used on the server.
    , channel       :: !Channel -- ^ The channel connected to.
    , password      :: !Password -- ^ The password to connect with.
    , pluginMonitor :: !DikuntMonitor -- ^ Handler of Dikunt plugins.
    , closedMVar    :: MVar () -- ^ When not empty bot thread is stopped.
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

{- | Represents an IRC user which consist of a nickname, an optional username
 - and an optional hostname. In messages from IRC servers the message has a user
 - iff it starts with a ':' character. The user is then parsed as
 - "nickname [ [ "!" user ] "@" host ]". -}
data IRCUser = IRCUser Nickname (Maybe Username) (Maybe Hostname)
  deriving (Show, Read, Eq)

{- | Convert an IRCUser to JSON. -}
instance ToJSON IRCUser where
    toJSON (IRCUser nick Nothing Nothing) = object
        [ "nickname" .= nick
        ]

    toJSON (IRCUser nick Nothing (Just host)) = object
        [ "nickname" .= nick
        , "hostname" .= host
        ]

    toJSON (IRCUser nick (Just user) Nothing) = object
        [ "nickname" .= nick
        , "username" .= user
        ]

    toJSON (IRCUser nick (Just user) (Just host)) = object
        [ "nickname" .= nick
        , "username" .= user
        , "hostname" .= host
        ]

{- | Read an IRCUser from JSON. -}
instance FromJSON IRCUser where
    parseJSON = withObject "IRCUser" $ \o ->
        IRCUser <$> o .: "nickname" <*> o .:? "username" <*> o .:? "hostname"

{- | Construct arbitrary IRC users for testing. -}
instance Arbitrary IRCUser where
    arbitrary = IRCUser <$> arbitrary <*> arbitrary <*> arbitrary

    shrink (IRCUser nick user host) = shrink3 IRCUser nick user host

data UserServer = UserServer Username (Maybe Hostname) (Maybe Servername)
  deriving (Show, Read, Eq)

instance ToJSON UserServer where
    toJSON (UserServer user Nothing Nothing) = object
        [ "username" .= user
        ]
    toJSON (UserServer user Nothing (Just server)) = object
        [ "username" .= user
        , "servername" .= server
        ]
    toJSON (UserServer user (Just host) Nothing) = object
        [ "username" .= user
        , "hostname" .= host
        ]
    toJSON (UserServer user (Just host) (Just server)) = object
        [ "username" .= user
        , "hostname" .= host
        , "servername" .= server
        ]

instance FromJSON UserServer where
    parseJSON = withObject "IRCUser" $ \o ->
        UserServer <$> o .: "username" <*> o .:? "hostname" <*> o .:? "servername"

instance Arbitrary UserServer where
    arbitrary = UserServer <$> arbitrary <*> arbitrary <*> arbitrary

{- | Messages that are received by the bot from the IRC server is parsed to this
 - structure. The structure contains almost all messages that can be send from
 - the server. -}
data ServerMessage
    -- | ServerNick <user> <nick> - user change nickname to nick.
    = ServerNick IRCUser Nickname
    -- | ServerQuit <user> <message> - user quit IRC.
    | ServerQuit IRCUser String
    -- | ServerJoin <user> <channel> - User joins channel.
    | ServerJoin IRCUser Channel
    -- | ServerPart <user> <channel> <message> - User leaves channel with the
    -- message.
    | ServerPart IRCUser Channel String
    -- | ServerTopic <user> <channel> <topic> - User set <topic> as topic for
    -- <channel>.
    | ServerTopic IRCUser Channel String
    -- | ServerInite <user> <nick> <channel> - User invited <nick> to <channel>.
    | ServerInvite IRCUser Nickname Channel
    -- | ServerKick <user> <channel> <nick> - <nick> kicked from <channel> by
    -- <user>.
    | ServerKick IRCUser Channel Nickname
    -- | ServerPrivMsg <user> <targets> <message> - Write <message> to <targets>
    -- from <user>.
    | ServerPrivMsg IRCUser [Target] String
    -- | ServerNotice <server> <targets> <message> - Write message from <server>
    -- to <targets>.
    | ServerNotice Servername [Target] String
    -- | ServerPing <server> - Ping coming from <server>.
    | ServerPing Servername
    -- | ServerMode <user> <nick> <mode> - User change mode <mode> for <nick>.
    | ServerMode IRCUser Nickname String
    -- | ServerReply <server> <num> <args> - Numeric reply <num> from
    -- <server> with arguments <args>.
    | ServerReply Servername Integer [String]
  deriving (Show, Eq)

{- | Convert a ServerMessage to a JSON string. -}
instance ToJSON ServerMessage where
    toJSON (ServerNick ircUser nick) = object
        [ "command" .= ("NICK" :: Text)
        , "ircUser" .= ircUser
        , "nickname" .= nick
        ]
    toJSON (ServerQuit ircUser reason) = object
        [ "command" .= ("QUIT" :: Text)
        , "ircUser" .= ircUser
        , "reason" .= reason
        ]
    toJSON (ServerJoin ircUser chan) = object
        [ "command" .= ("JOIN" :: Text)
        , "ircUser" .= ircUser
        , "channel" .= chan
        ]
    toJSON (ServerPart ircUser chan reason) = object
        [ "command" .= ("PART" :: Text)
        , "ircUser" .= ircUser
        , "channel" .= chan
        , "reason" .= reason
        ]
    toJSON (ServerTopic ircUser chan topic) = object
        [ "command" .= ("TOPIC" :: Text)
        , "ircUser" .= ircUser
        , "channel" .= chan
        , "topic" .= topic
        ]
    toJSON (ServerInvite ircUser nick chan) = object
        [ "command" .= ("INVITE" :: Text)
        , "ircUser" .= ircUser
        , "nickname" .= nick
        , "channel" .= chan
        ]
    toJSON (ServerKick ircUser chan nick) = object
        [ "command" .= ("KICK" :: Text)
        , "ircUser" .= ircUser
        , "channel" .= chan
        , "nickname" .= nick
        ]
    toJSON (ServerPrivMsg ircUser targets message) = object
        [ "command" .= ("PRIVMSG" :: Text)
        , "ircUser" .= ircUser
        , "targets" .= targets
        , "message" .= message
        ]
    toJSON (ServerNotice server targets message) = object
        [ "command" .= ("NOTICE" :: Text)
        , "server" .= server
        , "targets" .= targets
        , "message" .= message
        ]
    toJSON (ServerPing servername) = object
        [ "command" .= ("PING" :: Text)
        , "servername" .= servername
        ]
    toJSON (ServerMode ircUser nick mode) = object
        [ "command" .= ("MODE" :: Text)
        , "ircUser" .= ircUser
        , "nickname" .= nick
        , "mode" .= mode
        ]
    toJSON (ServerReply servername numeric args) = object
        [ "command" .= ("REPLY" :: Text)
        , "servername" .= servername
        , "numeric" .= numeric
        , "args" .= args
        ]

{- | Read a ServerMessage from a JSON string. -}
instance FromJSON ServerMessage where
    parseJSON = withObject "ServerMessage" $ \o -> do
        command <- o .: "command"
        case command of
            "NICK" -> do
                ircUser <- o .: "ircUser"
                nick <- o .: "nickname"
                return $ ServerNick ircUser nick
            "QUIT" -> do
                ircUser <- o .: "ircUser"
                reason <- o .: "reason"
                return $ ServerQuit ircUser reason
            "JOIN" -> do
                ircUser <- o .: "ircUser"
                chan <- o .: "channel"
                return $ ServerJoin ircUser chan
            "PART" -> do
                ircUser <- o .: "ircUser"
                chan <- o .: "channel"
                reason <- o .: "reason"
                return $ ServerPart ircUser chan reason
            "TOPIC" -> do
                ircUser <- o .: "ircUser"
                chan <- o .: "channel"
                topic <- o .: "topic"
                return $ ServerTopic ircUser chan topic
            "INVITE" -> do
                ircUser <- o .: "ircUser"
                nick <- o .: "nickname"
                chan <- o .: "channel"
                return $ ServerInvite ircUser nick chan
            "KICK" -> do
                ircUser <- o .: "ircUser"
                chan <- o .: "channel"
                nick <- o .: "nickname"
                return $ ServerKick ircUser chan nick
            "PRIVMSG" -> do
                ircUser <- o .: "ircUser"
                targets <- o .: "targets"
                message <- o .: "message"
                return $ ServerPrivMsg ircUser targets message
            "NOTICE" -> do
                servername <- o .: "servername"
                message <- o .: "message"
                targets <- o .: "targets"
                return $ ServerNotice servername targets message
            "PING" -> do
                servername <- o .: "servername"
                return $ ServerPing servername
            "MODE" -> do
                ircUser <- o .: "ircUser"
                nick <- o .: "nickname"
                mode <- o .: "mode"
                return $ ServerMode ircUser nick mode
            "REPLY" -> do
                servername <- o .: "servername"
                numeric <- o .: "numeric"
                args <- o .: "args"
                return $ ServerReply servername numeric args
            _ -> fail $ "Unknown command " ++ command

{- | Construct an arbitrary ServerMessage for testing. -}
instance Arbitrary ServerMessage where
    arbitrary = oneof
        [ ServerNick <$> arbitrary <*> arbitrary
        , ServerQuit <$> arbitrary <*> arbitrary
        , ServerJoin <$> arbitrary <*> arbitrary
        , ServerPart <$> arbitrary <*> arbitrary <*> arbitrary
        , ServerTopic <$> arbitrary <*> arbitrary <*> arbitrary
        , ServerInvite <$> arbitrary <*> arbitrary <*> arbitrary
        , ServerKick <$> arbitrary <*> arbitrary <*> arbitrary
        , ServerPrivMsg <$> arbitrary <*> arbitrary <*> arbitrary
        , ServerNotice <$> arbitrary <*> arbitrary <*> arbitrary
        , ServerPing <$> arbitrary
        , ServerMode <$> arbitrary <*> arbitrary <*> arbitrary
        , ServerReply <$> arbitrary <*> arbitrary <*> arbitrary
        ]

    shrink (ServerNick user nick) = shrink2 ServerNick user nick
    shrink (ServerQuit user reason) = shrink2 ServerQuit user reason
    shrink (ServerJoin user chan) = shrink2 ServerJoin user chan
    shrink (ServerPart user chan reason) = shrink3 ServerPart user chan reason
    shrink (ServerTopic user chan topic) = shrink3 ServerTopic user chan topic
    shrink (ServerInvite user nick chan) = shrink3 ServerInvite user nick chan
    shrink (ServerKick user chan nick) = shrink3 ServerKick user chan nick
    shrink (ServerPrivMsg user ts msg) = shrink3 ServerPrivMsg user ts msg
    shrink (ServerNotice server ts msg) = shrink3 ServerNotice server ts msg
    shrink (ServerPing server) = shrink1 ServerPing server
    shrink (ServerMode user nick mode) = shrink3 ServerMode user nick mode
    shrink (ServerReply server num args) = shrink3 ServerReply server num args

data Target = ChannelTarget Channel
    | UserTarget UserServer
    | NickTarget IRCUser
  deriving (Show, Eq)

instance ToJSON Target where
    toJSON (ChannelTarget chan) = object
        [ "channel" .= chan
        ]
    toJSON (UserTarget user) = object
        [ "username" .= user
        ]
    toJSON (NickTarget user) = object
        [ "nickname" .= user
        ]

instance FromJSON Target where
    parseJSON = withObject "Target" $ \o -> do
        chan <- o .:? "channel"
        user <- o .:? "username"
        nick <- o .:? "nickname"
        case (chan, user, nick) of
            (Just c, Nothing, Nothing) -> return $ ChannelTarget c
            (Nothing, Just u, Nothing) -> return $ UserTarget u
            (Nothing, Nothing, Just n) -> return $ NickTarget n
            _ -> fail "Should be either channel, username or nickname"

instance Arbitrary Target where
    arbitrary = oneof
        [ ChannelTarget <$> arbitrary
        , UserTarget <$> arbitrary
        , NickTarget <$> arbitrary
        ]

    shrink (ChannelTarget c) = [ChannelTarget c' | c' <- shrink c]
    shrink (UserTarget u) = [UserTarget u' | u' <- shrink u]
    shrink (NickTarget n) = [NickTarget n' | n' <- shrink n]

{- | Represent messages that can be send to the server. The messages can be
 - written as the string to be send to an IRC server with the function
 - IRCWriter.IRCWriter.writeMessage. -}
data ClientMessage = ClientPass Password
    | ClientNick Nickname
    | ClientUser Username Mode Realname
    | ClientOper Username Password
    | ClientMode Nickname String
    | ClientQuit String -- Reason for leaving.
    | ClientJoin [(Channel, String)]
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

{- | Convert a ClientMessage to a JSON string. -}
instance ToJSON ClientMessage where
    toJSON (ClientPass pass) = object
        [ "command" .= ("PASS" :: Text)
        , "password" .= pass
        ]
    toJSON (ClientNick nick) = object
        [ "command" .= ("NICK" :: Text)
        , "nickname" .= nick
        ]
    toJSON (ClientUser user mode realname) = object
        [ "command" .= ("USER" :: Text)
        , "username" .= user
        , "mode" .= mode
        , "realname" .= realname
        ]
    toJSON (ClientOper user pass) = object
        [ "command" .= ("OPER" :: Text)
        , "username" .= user
        , "password" .= pass
        ]
    toJSON (ClientMode nick mode) = object
        [ "command" .= ("MODE" :: Text)
        , "nickname" .= nick
        , "mode" .= mode
        ]
    toJSON (ClientQuit reason) = object
        [ "command" .= ("QUIT" :: Text)
        , "reason" .= reason
        ]
    toJSON (ClientJoin channelKeys) = object
        [ "command" .= ("JOIN" :: Text)
        , "channelkeys" .= map toChannelKey channelKeys
        ]
    toJSON (ClientPart chans reason) = object
        [ "command" .= ("PART" :: Text)
        , "channels" .= chans
        , "reason" .= reason
        ]
    toJSON (ClientTopic chan topic) = object
        [ "command" .= ("TOPIC" :: Text)
        , "channel" .= chan
        , "topic" .= topic
        ]
    toJSON (ClientNames chans) = object
        [ "command" .= ("NAMES" :: Text)
        , "channels" .= chans
        ]
    toJSON (ClientList chans) = object
        [ "command" .= ("LIST" :: Text)
        , "channels" .= chans
        ]
    toJSON (ClientInvite nick chan) = object
        [ "command" .= ("INVITE" :: Text)
        , "nickname" .= nick
        , "channel" .= chan
        ]
    toJSON (ClientPrivMsg ircUser message) = object
        [ "command" .= ("PRIVMSG" :: Text)
        , "ircUser" .= ircUser
        , "message" .= message
        ]
    toJSON (ClientPrivMsgChan chan message) = object
        [ "command" .= ("PRIVMSG" :: Text)
        , "channel" .= chan
        , "message" .= message
        ]
    toJSON (ClientNotice ircUser message) = object
        [ "command" .= ("NOTICE" :: Text)
        , "ircUser" .= ircUser
        , "message" .= message
        ]
    toJSON (ClientWho mask) = object
        [ "command" .= ("WHO" :: Text)
        , "mask" .= mask
        ]
    toJSON (ClientWhoIs servername username) = object
        [ "command" .= ("WHOIS" :: Text)
        , "servername" .= servername
        , "username" .= username
        ]
    toJSON (ClientWhoWas username count servername) = object
        [ "command" .= ("WHOWAS" :: Text)
        , "username" .= username
        , "count" .= count
        , "servername" .= servername
        ]
    toJSON (ClientPing servername) = object
        [ "command" .= ("PING" :: Text)
        , "servername" .= servername
        ]
    toJSON (ClientPong servername) = object
        [ "command" .= ("PONG" :: Text)
        , "servername" .= servername
        ]

{- Convert a JSON string to a ClientMessage. -}
instance FromJSON ClientMessage where
    parseJSON = withObject "ClientMessage" $ \o -> do
        command <- o .: "command"
        case command of
            "PASS" -> do
                pass <- o .: "password"
                return $ ClientPass pass
            "NICK" -> do
                nick <- o .: "nickname"
                return $ ClientNick nick
            "USER" -> do
                username <- o .: "username"
                mode <- o .: "mode"
                realname <- o .: "realname"
                return $ ClientUser username mode realname
            "OPER" -> do
                user <- o .: "username"
                pass <- o .: "password"
                return $ ClientOper user pass
            "MODE" -> do
                nick <- o .: "nickname"
                mode <- o .: "mode"
                return $ ClientMode nick mode
            "QUIT" -> do
                reason <- o .: "reason"
                return $ ClientQuit reason
            "JOIN" -> do
                channelKeys <- o .: "channelkeys"
                return $ ClientJoin (map fromChannelKey channelKeys)
            "PART" -> do
                channels <- o .: "channels"
                reason <- o .: "reason"
                return $ ClientPart channels reason
            "TOPIC" -> do
                chan <- o .: "channel"
                topic <- o .: "topic"
                return $ ClientTopic chan topic
            "NAMES" -> do
                channels <- o .: "channels"
                return $ ClientNames channels
            "LIST" -> do
                channels <- o .: "channels"
                return $ ClientList channels
            "INVITE" -> do
                nick <- o .: "nickname"
                chan <- o .: "channel"
                return $ ClientInvite nick chan
            "PRIVMSG" -> do
                ircUser <- o .:? "ircUser"
                chan <- o .:? "channel"
                message <- o .: "message"
                case (ircUser, chan) of
                    (Just u, Nothing) -> return $ ClientPrivMsg u message
                    (Nothing, Just c) -> return $ ClientPrivMsgChan c message
                    _ -> fail "Either both user and chan or none of them"
            "NOTICE" -> do
                ircUser <- o .: "ircUser"
                message <- o .: "message"
                return $ ClientNotice ircUser message
            "WHO" -> do
                mask <- o .: "mask"
                return $ ClientWho mask
            "WHOIS" -> do
                servername <- o .: "servername"
                user <- o .: "username"
                return $ ClientWhoIs servername user
            "WHOWAS" -> do
                user <- o .: "username"
                count <- o .:? "count"
                servername <- o .:? "servername"
                return $ ClientWhoWas user count servername
            "PING" -> do
                servername <- o .: "servername"
                return $ ClientPing servername
            "PONG" -> do
                servername <- o .: "servername"
                return $ ClientPong servername
            _ -> fail $ "Unknown command " ++ command

{- | Construct an arbitrary ClientMessage for testing. -}
instance Arbitrary ClientMessage where
    arbitrary = oneof
        [ ClientPass <$> arbitrary
        , ClientNick <$> arbitrary
        , ClientUser <$> arbitrary <*> arbitrary <*> arbitrary
        , ClientOper <$> arbitrary <*> arbitrary
        , ClientMode <$> arbitrary <*> arbitrary
        , ClientQuit <$> arbitrary
        , ClientJoin <$> arbitrary
        , ClientPart <$> arbitrary <*> arbitrary
        , ClientTopic <$> arbitrary <*> arbitrary
        , ClientNames <$> arbitrary
        , ClientList <$> arbitrary
        , ClientInvite <$> arbitrary <*> arbitrary
        , ClientPrivMsg <$> arbitrary <*> arbitrary
        , ClientPrivMsgChan <$> arbitrary <*> arbitrary
        , ClientNotice <$> arbitrary <*> arbitrary
        , ClientWho <$> arbitrary
        , ClientWhoIs <$> arbitrary <*> arbitrary
        , ClientWhoWas <$> arbitrary <*> arbitrary <*> arbitrary
        , ClientPing <$> arbitrary
        , ClientPong <$> arbitrary
        ]

    shrink (ClientPass pass) = shrink1 ClientPass pass
    shrink (ClientNick nick) = shrink1 ClientNick nick
    shrink (ClientUser user mode real) = shrink3 ClientUser user mode real
    shrink (ClientOper user pass) = shrink2 ClientOper user pass
    shrink (ClientMode nick mode) = shrink2 ClientMode nick mode
    shrink (ClientQuit reason) = shrink1 ClientQuit reason
    shrink (ClientJoin channelKeys) = shrink1 ClientJoin channelKeys
    shrink (ClientPart channels reason) = shrink2 ClientPart channels reason
    shrink (ClientTopic chan topic) = shrink2 ClientTopic chan topic
    shrink (ClientNames channels) = shrink1 ClientNames channels
    shrink (ClientList channels) = shrink1 ClientList channels
    shrink (ClientInvite nick chan) = shrink2 ClientInvite nick chan
    shrink (ClientPrivMsg user message) = shrink2 ClientPrivMsg user message
    shrink (ClientPrivMsgChan chan message) = shrink2 ClientPrivMsgChan chan message
    shrink (ClientNotice user message) = shrink2 ClientNotice user message
    shrink (ClientWho mask) = shrink1 ClientWho mask
    shrink (ClientWhoIs server user) = shrink2 ClientWhoIs server user
    shrink (ClientWhoWas user limit server) = shrink3 ClientWhoWas user limit server
    shrink (ClientPing server) = shrink1 ClientPing server
    shrink (ClientPong server) = shrink1 ClientPong server

{- | Construct a Bot to handle a IRC chat. -}
bot :: Handle
    -- ^ Socket to an IRC server.
    -> Nickname
    -- ^ Nickname to use on the server.
    -> Channel
    -- ^ Channel to connect to, should start with a #.
    -> Password
    -- ^ Password to use.
    -> DikuntMonitor
    -- ^ Input and output file handles for plugins.
    -> MVar ()
    -- ^ Empty MVar used to communicate with bot threads.
    -> Bot
bot = Bot

{- | Get the IRC command of a server message. -}
getServerCommand :: ServerMessage
    -- ^ Message to get command from.
    -> IRCCommand
getServerCommand ServerNick{} = NICK
getServerCommand ServerJoin{} = JOIN
getServerCommand ServerQuit{} = QUIT
getServerCommand ServerKick{} = KICK
getServerCommand ServerPart{} = PART
getServerCommand ServerTopic{} = TOPIC
getServerCommand ServerInvite{} = INVITE
getServerCommand ServerPrivMsg{} = PRIVMSG
getServerCommand ServerNotice{} = NOTICE
getServerCommand ServerPing{} = PING
getServerCommand ServerMode{} = MODE
getServerCommand (ServerReply _ n _) = NUMCOM n

{- | Get the IRC command of a client message. -}
getClientCommand :: ClientMessage
    -- ^ Message to get command from.
    -> IRCCommand
getClientCommand ClientPass{} = PASS
getClientCommand ClientNick{} = NICK
getClientCommand ClientUser{} = USER
getClientCommand ClientOper{} = OPER
getClientCommand ClientMode{} = MODE
getClientCommand ClientQuit{} = QUIT
getClientCommand ClientJoin{} = JOIN
getClientCommand ClientPart{} = PART
getClientCommand ClientTopic{} = TOPIC
getClientCommand ClientNames{} = NAMES
getClientCommand ClientList{} = LIST
getClientCommand ClientInvite{} = INVITE
getClientCommand ClientPrivMsg{} = PRIVMSG
getClientCommand ClientPrivMsgChan{} = PRIVMSG
getClientCommand ClientNotice{} = NOTICE
getClientCommand ClientWho{} = WHO
getClientCommand ClientWhoIs{} = WHOIS
getClientCommand ClientWhoWas{} = WHOWAS
getClientCommand ClientPing{} = PING
getClientCommand ClientPong{} = PONG

{- | Helper data type to convert a list of tuples to JSON with named fields. -}
data ChannelKey = ChannelKey Channel String

{- | Convert ChannelKey to a JSON string. -}
instance ToJSON ChannelKey where
    toJSON (ChannelKey chan key) = object
        [ "channel" .= chan
        , "key" .= key
        ]

{- | Read a ChannelKey from a JSON string. -}
instance FromJSON ChannelKey where
    parseJSON = withObject "ChannelKey" $ \o ->
        ChannelKey <$> o .: "channel" <*> o .: "key"

{- | Construct a channel key. -}
toChannelKey :: (Channel, String)
    -- ^ Tuple containing channel and key.
    -> ChannelKey
toChannelKey (chan, key) = ChannelKey chan key

{- | Deconstruct a channel key. -}
fromChannelKey :: ChannelKey
    -- ^ ChannelKey to deconstruct.
    -> (Channel, String)
fromChannelKey (ChannelKey chan key) = (chan, key)
