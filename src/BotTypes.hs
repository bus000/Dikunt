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
    , IRCMessage(..)
    , IRCCommand(..)
    , IRCPrefix(..)
    , IRCUser(..)
    , ServerMessage(..)
    , ClientMessage(..)
    , nicknamePrefix
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

import Data.Aeson (ToJSON(..), FromJSON(..), withObject, (.=), (.:), (.:?), object, withText)
import qualified Data.Aeson.Types as Aeson
import Text.Regex.PCRE ((=~))
import Data.Text (Text)
import Monitoring (DikuntMonitor)
import System.IO (Handle)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)
import Test.QuickCheck.Gen (oneof, suchThat)
import Control.Concurrent.MVar (MVar)
import qualified Data.Text as T

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
    arbitrary = do
        nick <- suchThat arbitrary (\s -> nickname s /= Nothing)
        return . Nickname $ nick

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
    arbitrary = do
        nick <- arbitrary
        maybeUser <- arbitrary
        maybeHost <- arbitrary

        return $ IRCUser nick maybeUser maybeHost

    shrink (IRCUser nick Nothing Nothing) =
        [IRCUser nick' Nothing Nothing | (nick') <- shrink nick]

    shrink (IRCUser nick Nothing (Just host)) = [IRCUser nick Nothing Nothing]
        ++ [IRCUser nick' Nothing (Just host') |
            (nick', host') <- shrink (nick, host)]

    shrink (IRCUser nick (Just user) Nothing) = [IRCUser nick Nothing Nothing]
        ++ [IRCUser nick' (Just user') Nothing |
            (nick', user') <- shrink (nick, user)]

    shrink (IRCUser nick (Just user) (Just host)) =
        [ IRCUser nick Nothing Nothing
        , IRCUser nick Nothing (Just host)
        , IRCUser nick (Just user) Nothing
        ] ++ [IRCUser nick' (Just user') (Just host') |
            (nick', user', host') <- shrink (nick, user, host)]

{- | Messages that are received by the bot from the IRC server is parsed to this
 - structure. The structure contains almost all messages that can be send from
 - the server. -}
data ServerMessage = ServerNick IRCUser Nickname
    | ServerJoin IRCUser Channel
    | ServerPart IRCUser Channel String
    | ServerQuit IRCUser String
    | ServerTopic IRCUser Channel String
    | ServerInvite IRCUser Nickname Channel
    | ServerPrivMsg IRCUser Channel String
    | ServerNotice IRCUser Channel String
    | ServerNoticeServer Servername Channel String
    | ServerPing Servername
    | ServerReply Servername Integer [String] (Maybe String)
    deriving (Eq, Show, Read)

{- | Convert a ServerMessage to a JSON string. -}
instance ToJSON ServerMessage where
    toJSON (ServerNick ircUser nick) = object
        [ "command" .= ("NICK" :: Text)
        , "ircUser" .= ircUser
        , "nickname" .= nick
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
    toJSON (ServerQuit ircUser reason) = object
        [ "command" .= ("QUIT" :: Text)
        , "ircUser" .= ircUser
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
    toJSON (ServerPrivMsg ircUser nick message) = object
        [ "command" .= ("PRIVMSG" :: Text)
        , "ircUser" .= ircUser
        , "nickname" .= nick
        , "message" .= message
        ]
    toJSON (ServerNotice ircUser nick message) = object
        [ "command" .= ("NOTICE" :: Text)
        , "ircUser" .= ircUser
        , "nickname" .= nick
        , "message" .= message
        ]
    toJSON (ServerNoticeServer servername chan message) = object
        [ "command" .= ("NOTICE" :: Text)
        , "servername" .= servername
        , "channel" .= chan
        , "message" .= message
        ]
    toJSON (ServerPing servername) = object
        [ "command" .= ("PING" :: Text)
        , "servername" .= servername
        ]
    toJSON (ServerReply servername numeric args (Just trailing)) = object
        [ "command" .= ("REPLY" :: Text)
        , "servername" .= servername
        , "numeric" .= numeric
        , "args" .= args
        , "trailing" .= trailing
        ]
    toJSON (ServerReply servername numeric args Nothing) = object
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
            "JOIN" -> do
                ircUser <- o .: "ircUser"
                chan <- o .: "channel"
                return $ ServerJoin ircUser chan
            "PART" -> do
                ircUser <- o .: "ircUser"
                chan <- o .: "channel"
                reason <- o .: "reason"
                return $ ServerPart ircUser chan reason
            "QUIT" -> do
                ircUser <- o .: "ircUser"
                reason <- o .: "reason"
                return $ ServerQuit ircUser reason
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
            "PRIVMSG" -> do
                ircUser <- o .: "ircUser"
                nick <- o .: "nickname"
                message <- o .: "message"
                return $ ServerPrivMsg ircUser nick message
            "NOTICE" -> do
                ircUser <- o .:? "ircUser"
                servername <- o .:? "servername"
                message <- o .: "message"
                case (ircUser, servername) of
                    (Just u, Nothing) -> do
                        nick <-  o .: "nickname"
                        return $ ServerNotice u nick message
                    (Nothing, Just s) -> do
                        chan <- o .: "channel"
                        return $ ServerNoticeServer s chan message
                    _ -> fail "Either user or servername"
            "PING" -> do
                servername <- o .: "servername"
                return $ ServerPing servername
            "REPLY" -> do
                servername <- o .: "servername"
                numeric <- o .: "numeric"
                args <- o .: "args"
                trailing <- o .:? "trailing"
                return $ ServerReply servername numeric args trailing
            _ -> fail $ "Unknown command " ++ command

{- | Construct an arbitrary ServerMessage for testing. -}
instance Arbitrary ServerMessage where
    arbitrary = oneof
        [ arbitrary >>= \(u, n)       -> return $ ServerNick u n
        , arbitrary >>= \(u, c)       -> return $ ServerJoin u c
        , arbitrary >>= \(u, c, r)    -> return $ ServerPart u c r
        , arbitrary >>= \(u, r)       -> return $ ServerQuit u r
        , arbitrary >>= \(u, c, t)    -> return $ ServerTopic u c t
        , arbitrary >>= \(u, n, c)    -> return $ ServerInvite u n c
        , arbitrary >>= \(u, n, m)    -> return $ ServerPrivMsg u n m
        , arbitrary >>= \(u, n, m)    -> return $ ServerNotice u n m
        , arbitrary >>= \(s, n, m)    -> return $ ServerNoticeServer s n m
        , arbitrary >>= \s            -> return $ ServerPing s
        , arbitrary >>= \(s, c, a, t) -> return $ ServerReply s c a t
        ]

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
        [ arbitrary >>= \p         -> return $ ClientPass p
        , arbitrary >>= \n         -> return $ ClientNick n
        , arbitrary >>= \(u, m, r) -> return $ ClientUser u m r
        , arbitrary >>= \(u, p)    -> return $ ClientOper u p
        , arbitrary >>= \(n, m)    -> return $ ClientMode n m
        , arbitrary >>= \r         -> return $ ClientQuit r
        , arbitrary >>= \c         -> return $ ClientJoin c
        , arbitrary >>= \(c, r)    -> return $ ClientPart c r
        , arbitrary >>= \(c, t)    -> return $ ClientTopic c t
        , arbitrary >>= \c         -> return $ ClientNames c
        , arbitrary >>= \c         -> return $ ClientList c
        , arbitrary >>= \(n, c)    -> return $ ClientInvite n c
        , arbitrary >>= \(u, m)    -> return $ ClientPrivMsg u m
        , arbitrary >>= \(c, m)    -> return $ ClientPrivMsgChan c m
        , arbitrary >>= \(u, m)    -> return $ ClientNotice u m
        , arbitrary >>= \w         -> return $ ClientWho w
        , arbitrary >>= \(s, u)    -> return $ ClientWhoIs s u
        , arbitrary >>= \(u, n, s) -> return $ ClientWhoWas u n s
        , arbitrary >>= \s         -> return $ ClientPing s
        , arbitrary >>= \s         -> return $ ClientPong s
        ]

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

{- | Get the IRC command of a server message. -}
getServerCommand :: ServerMessage
    -- ^ Message to get command from.
    -> IRCCommand
getServerCommand (ServerNick _ _) = NICK
getServerCommand (ServerJoin _ _) = JOIN
getServerCommand (ServerPart _ _ _) = PART
getServerCommand (ServerQuit _ _) = QUIT
getServerCommand (ServerTopic _ _ _) = TOPIC
getServerCommand (ServerInvite _ _ _) = INVITE
getServerCommand (ServerPrivMsg _ _ _) = PRIVMSG
getServerCommand (ServerNotice _ _ _) = NOTICE
getServerCommand (ServerNoticeServer _ _ _) = NOTICE
getServerCommand (ServerPing _) = PING
getServerCommand (ServerReply _ n _ _) = NUMCOM n

{- | Get the IRC command of a client message. -}
getClientCommand :: ClientMessage
    -- ^ Message to get command from.
    -> IRCCommand
getClientCommand (ClientPass _) = PASS
getClientCommand (ClientNick _) = NICK
getClientCommand (ClientUser _ _ _) = USER
getClientCommand (ClientOper _ _) = OPER
getClientCommand (ClientMode _ _) = MODE
getClientCommand (ClientQuit _) = QUIT
getClientCommand (ClientJoin _) = JOIN
getClientCommand (ClientPart _ _) = PART
getClientCommand (ClientTopic _ _) = TOPIC
getClientCommand (ClientNames _) = NAMES
getClientCommand (ClientList _) = LIST
getClientCommand (ClientInvite _ _) = INVITE
getClientCommand (ClientPrivMsg _ _) = PRIVMSG
getClientCommand (ClientPrivMsgChan _ _) = PRIVMSG
getClientCommand (ClientNotice _ _) = NOTICE
getClientCommand (ClientWho _) = WHO
getClientCommand (ClientWhoIs _ _) = WHOIS
getClientCommand (ClientWhoWas _ _ _) = WHOWAS
getClientCommand (ClientPing _) = PING
getClientCommand (ClientPong _) = PONG

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
