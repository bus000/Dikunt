{- |
 - Module      : Types.Internal
 - Description : Internal implementation of types from Types.BotTypes.
 - Copyright   : (c) Magnus Stavngaard, 2016
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -}
{-# LANGUAGE OverloadedStrings #-}
module Types.Internal where

import Control.Concurrent.MVar (MVar)
import Data.Aeson (ToJSON(..), FromJSON(..), withObject, (.=), (.:), (.:?), object)
import Data.Text (Text)
import Monitoring (DikuntMonitor)
import System.IO (Handle)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)
import Test.QuickCheck.Gen (oneof)
import Types.Internal.Channel (Channel, channel)
import Types.Internal.IRCUser (IRCUser(..))
import Types.Internal.Nickname (Nickname, nickname)
import Types.Internal.ServerMessage (ServerMessage(..))
import Types.Internal.Servername (Servername, servername)
import Types.Internal.Target (Targets)
import Types.Internal.Username (Username)
import Utils (shrink1, shrink2, shrink3)

{- | IRC password. -}
type Password = String

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

{- | Construct a bot configuration from a servername, nickname, password,
 - channel and port number. -}
botConfig :: String
    -- ^ Name or IP of server to connect to.
    -> String
    -- ^ Nickname of the bot.
    -> String
    -- ^ Password for NickServ.
    -> String
    -- ^ Channel to connect to.
    -> Integer
    -- ^ Port to connect to.
    -> Maybe BotConfig
botConfig serv nick pass chan port = BotConfig <$> servername serv <*>
    nickname nick <*> return pass <*> channel chan <*> return port

{- | The IRC bot parameters. -}
data Bot = Bot
    { socket        :: !Handle -- ^ The handle to communicate with the server.
    , botNickname   :: !Nickname -- ^ The nickname used on the server.
    , botChannel    :: !Channel -- ^ The channel connected to.
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
    toJSON (ClientWhoIs server user) = object
        [ "command" .= ("WHOIS" :: Text)
        , "servername" .= server
        , "username" .= user
        ]
    toJSON (ClientWhoWas user count server) = object
        [ "command" .= ("WHOWAS" :: Text)
        , "username" .= user
        , "count" .= count
        , "servername" .= server
        ]
    toJSON (ClientPing server) = object
        [ "command" .= ("PING" :: Text)
        , "servername" .= server
        ]
    toJSON (ClientPong server) = object
        [ "command" .= ("PONG" :: Text)
        , "servername" .= server
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
                user <- o .: "username"
                mode <- o .: "mode"
                realname <- o .: "realname"
                return $ ClientUser user mode realname
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
                server <- o .: "servername"
                user <- o .: "username"
                return $ ClientWhoIs server user
            "WHOWAS" -> do
                user <- o .: "username"
                count <- o .:? "count"
                server <- o .:? "servername"
                return $ ClientWhoWas user count server
            "PING" -> do
                server <- o .: "servername"
                return $ ClientPing server
            "PONG" -> do
                server <- o .: "servername"
                return $ ClientPong server
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
