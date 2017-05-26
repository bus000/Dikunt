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

    -- Bot configuration declaration.
    , BotConfig(..)

    -- Extra type declarations.
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
import Data.Aeson
    ( ToJSON(..)
    , FromJSON(..)
    , withObject
    , (.=)
    , (.:)
    , (.:?)
    , object
    )
import qualified Data.Text.Lazy as T
import Monitoring (Monitor)
import System.IO (Handle)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)
import Test.QuickCheck.Gen (oneof)

{- | IRC nickname. -}
type Nickname = String

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
    , _trail      :: Maybe T.Text -- ^ Actual message.
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
    | ServerPart IRCUser Channel T.Text
    | ServerQuit IRCUser T.Text
    | ServerTopic IRCUser Channel T.Text
    | ServerInvite IRCUser Nickname Channel
    | ServerPrivMsg IRCUser Nickname T.Text
    | ServerNotice IRCUser Nickname T.Text
    | ServerPing Servername
    | ServerReply Servername Integer [String] (Maybe T.Text)
    deriving (Eq, Show, Read)

{- | Convert a ServerMessage to a JSON string. -}
instance ToJSON ServerMessage where
    toJSON (ServerNick ircUser nick) = object
        [ "command" .= ("NICK" :: T.Text)
        , "ircUser" .= ircUser
        , "nickname" .= nick
        ]
    toJSON (ServerJoin ircUser chan) = object
        [ "command" .= ("JOIN" :: T.Text)
        , "ircUser" .= ircUser
        , "channel" .= chan
        ]
    toJSON (ServerPart ircUser chan reason) = object
        [ "command" .= ("PART" :: T.Text)
        , "ircUser" .= ircUser
        , "channel" .= chan
        , "reason" .= reason
        ]
    toJSON (ServerQuit ircUser reason) = object
        [ "command" .= ("QUIT" :: T.Text)
        , "ircUser" .= ircUser
        , "reason" .= reason
        ]
    toJSON (ServerTopic ircUser chan topic) = object
        [ "command" .= ("TOPIC" :: T.Text)
        , "ircUser" .= ircUser
        , "channel" .= chan
        , "topic" .= topic
        ]
    toJSON (ServerInvite ircUser nick chan) = object
        [ "command" .= ("INVITE" :: T.Text)
        , "ircUser" .= ircUser
        , "nickname" .= nick
        , "channel" .= chan
        ]
    toJSON (ServerPrivMsg ircUser nick message) = object
        [ "command" .= ("PRIVMSG" :: T.Text)
        , "ircUser" .= ircUser
        , "nickname" .= nick
        , "message" .= message
        ]
    toJSON (ServerNotice ircUser nick message) = object
        [ "command" .= ("NOTICE" :: T.Text)
        , "ircUser" .= ircUser
        , "nickname" .= nick
        , "message" .= message
        ]
    toJSON (ServerPing servername) = object
        [ "command" .= ("PING" :: T.Text)
        , "servername" .= servername
        ]
    toJSON (ServerReply servername numeric args (Just trailing)) = object
        [ "command" .= ("REPLY" :: T.Text)
        , "servername" .= servername
        , "numeric" .= numeric
        , "args" .= args
        , "trailing" .= trailing
        ]
    toJSON (ServerReply servername numeric args Nothing) = object
        [ "command" .= ("REPLY" :: T.Text)
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
                ircUser <- o .: "ircUser"
                nick <- o .: "nickname"
                message <- o .: "message"
                return $ ServerNotice ircUser nick message
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
        , arbitrary >>= \(u, c, r)    -> return $ ServerPart u c (fromMyText r)
        , arbitrary >>= \(u, r)       -> return $ ServerQuit u (fromMyText r)
        , arbitrary >>= \(u, c, t)    -> return $ ServerTopic u c (fromMyText t)
        , arbitrary >>= \(u, n, c)    -> return $ ServerInvite u n c
        , arbitrary >>= \(u, n, m)    -> return $ ServerPrivMsg u n (fromMyText m)
        , arbitrary >>= \(u, n, m)    -> return $ ServerNotice u n (fromMyText m)
        , arbitrary >>= \s            -> return $ ServerPing s
        , arbitrary >>= \(s, c, a, t) -> return $ ServerReply s c a (fmap fromMyText t)
        ]

{- | Represent messages that can be send to the server. The messages can be
 - written as the string to be send to an IRC server with the function
 - IRCWriter.IRCWriter.writeMessage. -}
data ClientMessage = ClientPass Password
    | ClientNick Nickname
    | ClientUser Username Mode Realname
    | ClientOper Username Password
    | ClientMode Nickname String
    | ClientQuit T.Text -- Reason for leaving.
    | ClientJoin [(Channel, String)]
    | ClientPart [Channel] (Maybe T.Text)
    | ClientTopic Channel (Maybe T.Text)
    | ClientNames [Channel]
    | ClientList [Channel]
    | ClientInvite Nickname Channel
    | ClientPrivMsg IRCUser T.Text
    | ClientPrivMsgChan Channel T.Text
    | ClientNotice IRCUser T.Text
    | ClientWho String
    | ClientWhoIs (Maybe Servername) Username
    | ClientWhoWas Username (Maybe Integer) (Maybe Servername)
    | ClientPing Servername
    | ClientPong Servername
    deriving (Eq, Show, Read)

{- | Convert a ClientMessage to a JSON string. -}
instance ToJSON ClientMessage where
    toJSON (ClientPass pass) = object
        [ "command" .= ("PASS" :: T.Text)
        , "password" .= pass
        ]
    toJSON (ClientNick nick) = object
        [ "command" .= ("NICK" :: T.Text)
        , "nickname" .= nick
        ]
    toJSON (ClientUser user mode realname) = object
        [ "command" .= ("USER" :: T.Text)
        , "username" .= user
        , "mode" .= mode
        , "realname" .= realname
        ]
    toJSON (ClientOper user pass) = object
        [ "command" .= ("OPER" :: T.Text)
        , "username" .= user
        , "password" .= pass
        ]
    toJSON (ClientMode nick mode) = object
        [ "command" .= ("MODE" :: T.Text)
        , "nickname" .= nick
        , "mode" .= mode
        ]
    toJSON (ClientQuit reason) = object
        [ "command" .= ("QUIT" :: T.Text)
        , "reason" .= reason
        ]
    toJSON (ClientJoin channelKeys) = object
        [ "command" .= ("JOIN" :: T.Text)
        , "channelkeys" .= map toChannelKey channelKeys
        ]
    toJSON (ClientPart chans reason) = object
        [ "command" .= ("PART" :: T.Text)
        , "channels" .= chans
        , "reason" .= reason
        ]
    toJSON (ClientTopic chan topic) = object
        [ "command" .= ("TOPIC" :: T.Text)
        , "channel" .= chan
        , "topic" .= topic
        ]
    toJSON (ClientNames chans) = object
        [ "command" .= ("NAMES" :: T.Text)
        , "channels" .= chans
        ]
    toJSON (ClientList chans) = object
        [ "command" .= ("LIST" :: T.Text)
        , "channels" .= chans
        ]
    toJSON (ClientInvite nick chan) = object
        [ "command" .= ("INVITE" :: T.Text)
        , "nickname" .= nick
        , "channel" .= chan
        ]
    toJSON (ClientPrivMsg ircUser message) = object
        [ "command" .= ("PRIVMSG" :: T.Text)
        , "ircUser" .= ircUser
        , "message" .= message
        ]
    toJSON (ClientPrivMsgChan chan message) = object
        [ "command" .= ("PRIVMSG" :: T.Text)
        , "channel" .= chan
        , "message" .= message
        ]
    toJSON (ClientNotice ircUser message) = object
        [ "command" .= ("NOTICE" :: T.Text)
        , "ircUser" .= ircUser
        , "message" .= message
        ]
    toJSON (ClientWho mask) = object
        [ "command" .= ("WHO" :: T.Text)
        , "mask" .= mask
        ]
    toJSON (ClientWhoIs servername username) = object
        [ "command" .= ("WHOIS" :: T.Text)
        , "servername" .= servername
        , "username" .= username
        ]
    toJSON (ClientWhoWas username count servername) = object
        [ "command" .= ("WHOWAS" :: T.Text)
        , "username" .= username
        , "count" .= count
        , "servername" .= servername
        ]
    toJSON (ClientPing servername) = object
        [ "command" .= ("PING" :: T.Text)
        , "servername" .= servername
        ]
    toJSON (ClientPong servername) = object
        [ "command" .= ("PONG" :: T.Text)
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
        , arbitrary >>= \r         -> return $ ClientQuit (fromMyText r)
        , arbitrary >>= \c         -> return $ ClientJoin c
        , arbitrary >>= \(c, r)    -> return $ ClientPart c (fmap fromMyText r)
        , arbitrary >>= \(c, t)    -> return $ ClientTopic c (fmap fromMyText t)
        , arbitrary >>= \c         -> return $ ClientNames c
        , arbitrary >>= \c         -> return $ ClientList c
        , arbitrary >>= \(n, c)    -> return $ ClientInvite n c
        , arbitrary >>= \(u, m)    -> return $ ClientPrivMsg u (fromMyText m)
        , arbitrary >>= \(c, m)    -> return $ ClientPrivMsgChan c (fromMyText m)
        , arbitrary >>= \(u, m)    -> return $ ClientNotice u (fromMyText m)
        , arbitrary >>= \w         -> return $ ClientWho w
        , arbitrary >>= \(s, u)    -> return $ ClientWhoIs s u
        , arbitrary >>= \(u, n, s) -> return $ ClientWhoWas u n s
        , arbitrary >>= \s         -> return $ ClientPing s
        , arbitrary >>= \s         -> return $ ClientPong s
        ]

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

newtype MyText = MyText T.Text

instance Arbitrary MyText where
    arbitrary = arbitrary >>= \str -> return $ MyText (T.pack str)

fromMyText :: MyText -> T.Text
fromMyText (MyText t) = t
