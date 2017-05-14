{-# LANGUAGE DeriveDataTypeable #-}
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

import Data.Text (Text)
import Control.Concurrent.MVar (MVar)
import System.IO (Handle)
import Monitoring (Monitor)
import Data.Aeson
    ( ToJSON(..)
    , FromJSON(..)
    , withObject
    , (.=)
    , (.:)
    , (.:?)
    , object
    )
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)
import Test.QuickCheck.Gen (oneof)

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

instance FromJSON IRCUser where
    parseJSON = withObject "IRCUser" $ \o ->
        IRCUser <$> o .: "nickname" <*> o .:? "username" <*> o .:? "hostname"

instance Arbitrary IRCUser where
    arbitrary = do
        nick <- arbitrary
        maybeUser <- arbitrary
        maybeHost <- arbitrary

        return $ IRCUser nick maybeUser maybeHost

    shrink (IRCUser nick Nothing Nothing) = []
    shrink (IRCUser nick Nothing (Just host)) = [IRCUser nick Nothing Nothing]
    shrink (IRCUser nick (Just user) Nothing) = [IRCUser nick Nothing Nothing]
    shrink (IRCUser nick (Just user) (Just host)) =
        [ IRCUser nick Nothing Nothing
        , IRCUser nick Nothing (Just host)
        , IRCUser nick (Just user) Nothing
        ]

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
                channel <- o .: "channel"
                return $ ServerJoin ircUser channel
            "PART" -> do
                ircUser <- o .: "ircUser"
                channel <- o .: "channel"
                reason <- o .: "reason"
                return $ ServerPart ircUser channel reason
            "QUIT" -> do
                ircUser <- o .: "ircUser"
                reason <- o .: "reason"
                return $ ServerQuit ircUser reason
            "TOPIC" -> do
                ircUser <- o .: "ircUser"
                channel <- o .: "channel"
                topic <- o .: "topic"
                return $ ServerTopic ircUser channel topic
            "INVITE" -> do
                ircUser <- o .: "ircUser"
                nickname <- o .: "nickname"
                channel <- o .: "channel"
                return $ ServerInvite ircUser nickname channel
            "PRIVMSG" -> do
                ircUser <- o .: "ircUser"
                nickname <- o .: "nickname"
                message <- o .: "message"
                return $ ServerPrivMsg ircUser nickname message
            "NOTICE" -> do
                ircUser <- o .: "ircUser"
                nickname <- o .: "nickname"
                message <- o .: "message"
                return $ ServerNotice ircUser nickname message
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

instance Arbitrary ServerMessage where
    arbitrary = do
        user <- arbitrary
        nick <- arbitrary
        chan <- arbitrary
        topic <- arbitrary
        reason <- arbitrary
        message <- arbitrary
        servername <- arbitrary
        code <- arbitrary
        args <- arbitrary
        trailing <- arbitrary

        oneof
            [ return $ ServerNick user nick
            , return $ ServerJoin user chan
            , return $ ServerPart user chan reason
            , return $ ServerQuit user reason
            , return $ ServerTopic user chan topic
            , return $ ServerInvite user nick chan
            , return $ ServerPrivMsg user nick message
            , return $ ServerNotice user nick message
            , return $ ServerPing servername
            , return $ ServerReply servername code args trailing
            ]

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
