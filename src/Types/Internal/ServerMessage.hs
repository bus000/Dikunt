{- |
 - Module      : Types.Internal.ServerMessage
 - Description : Define type representing any message sent from the server.
 - Copyright   : (c) Magnus Stavngaard, 2017
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - Define a type that represent any message that can be sent from the IRC
 - server.
 -}
{-# LANGUAGE OverloadedStrings #-}
module Types.Internal.ServerMessage where

import Data.Aeson (ToJSON(..), FromJSON(..), withObject, (.=), (.:), object)
import Data.Maybe (mapMaybe, isJust)
import Data.Text (Text)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)
import Test.QuickCheck.Gen (oneof, listOf1, suchThat)
import Types.Internal.Channel (Channel)
import Types.Internal.IRCUser (IRCUser)
import Types.Internal.Message (Message)
import Types.Internal.Nickname (Nickname)
import Types.Internal.Servername (Servername)
import Types.Internal.Target (Targets)
import Utils (shrink1, shrink2, shrink3)

{- | Messages that are received by the bot from the IRC server is parsed to this
 - structure. The structure contains almost all messages that can be send from
 - the server. -}
data ServerMessage
    -- | ServerNick <user> <nick> - user change nickname to nick.
    = ServerNick IRCUser Nickname
    -- | ServerQuit <user> <message> - user quit IRC.
    | ServerQuit IRCUser Message
    -- | ServerJoin <user> <channel> - User joins channel.
    | ServerJoin IRCUser Channel
    -- | ServerPart <user> <channel> <message> - User leaves channel with the
    -- message.
    | ServerPart IRCUser Channel Message
    -- | ServerTopic <user> <channel> <topic> - User set <topic> as topic for
    -- <channel>.
    | ServerTopic IRCUser Channel Message
    -- | ServerInite <user> <nick> <channel> - User invited <nick> to <channel>.
    | ServerInvite IRCUser Nickname Channel
    -- | ServerKick <user> <channel> <nick> - <nick> kicked from <channel> by
    -- <user>.
    | ServerKick IRCUser Channel Nickname
    -- | ServerPrivMsg <user> <targets> <message> - Write <message> to <targets>
    -- from <user>.
    | ServerPrivMsg IRCUser Targets Message
    -- | ServerNotice <server> <targets> <message> - Write message from <server>
    -- to <targets>.
    | ServerNotice Servername Targets Message
    -- | ServerPing <server> - Ping coming from <server>.
    | ServerPing Servername
    -- | ServerMode <user> <nick> <mode> - User change mode <mode> for <nick>.
    | ServerMode IRCUser Nickname Message
    -- | ServerReply <server> <num> <args> - Numeric reply <num> from
    -- <server> with arguments <args>.
    | ServerReply Servername Integer Arguments
  deriving (Show, Read, Eq)

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
        , "servername" .= server
        , "targets" .= targets
        , "message" .= message
        ]
    toJSON (ServerPing server) = object
        [ "command" .= ("PING" :: Text)
        , "servername" .= server
        ]
    toJSON (ServerMode ircUser nick mode) = object
        [ "command" .= ("MODE" :: Text)
        , "ircUser" .= ircUser
        , "nickname" .= nick
        , "mode" .= mode
        ]
    toJSON (ServerReply server numeric args) = object
        [ "command" .= ("REPLY" :: Text)
        , "servername" .= server
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
                server <- o .: "servername"
                message <- o .: "message"
                targets <- o .: "targets"
                return $ ServerNotice server targets message
            "PING" -> do
                server <- o .: "servername"
                return $ ServerPing server
            "MODE" -> do
                ircUser <- o .: "ircUser"
                nick <- o .: "nickname"
                mode <- o .: "mode"
                return $ ServerMode ircUser nick mode
            "REPLY" -> do
                server <- o .: "servername"
                numeric <- o .: "numeric"
                args <- o .: "args"
                return $ ServerReply server numeric args
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
        , ServerReply <$> arbitrary <*> (abs <$> arbitrary) <*> arbitrary
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
    shrink (ServerReply server num args) =
        (ServerReply <$> shrink server <*> return num <*> return args) ++
        (ServerReply <$> return server <*> shrink num <*> return args) ++
        (ServerReply <$> return server <*> return num <*> shrink args)

newtype Arguments = Arguments [String] deriving (Show, Read, Eq)

arguments :: [String] -> Maybe Arguments
arguments args | null args = Nothing
arguments args | any null args = Nothing
arguments args | any (any (`elem` (" \r\n\0" :: String))) args = Nothing
arguments args | otherwise = Just $ Arguments args

getArguments :: Arguments -> [String]
getArguments (Arguments args) = args

instance Arbitrary Arguments where
    arbitrary = Arguments <$> listOf1 (listOf1 arbitrary) `suchThat`
        (isJust . arguments)

    shrink = mapMaybe arguments . shrink . getArguments

instance ToJSON Arguments where
    toJSON (Arguments args) = toJSON args

instance FromJSON Arguments where
    parseJSON x = Arguments <$> parseJSON x
