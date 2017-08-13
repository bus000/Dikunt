{- |
 - Module      : IRCWriter.Impl
 - Description : Write IRC messages defined in BotTypes module as strings.
 - Copyright   : (c) Magnus Stavngaard, 2016
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - Exports functions that can write IRC messages as strings ready to be send to
 - IRC channels. The writing of server messages is mostly for testing as the bot
 - will probably never run as a server.
 -}
module IRCWriter.Impl where

import Data.List (intercalate)
import qualified Types.BotTypes as BT
import Types.BotTypes (getNickname, getChannel, getServerName)

{- | Convert client messages to strings that can be send to the IRC server. -}
writeMessage :: BT.ClientMessage
    -- ^ The message to convert to a string.
    -> String
writeMessage msg = writeMessage' msg ++ "\r\n"
  where
    writeMessage' (BT.ClientPass pass) =
        "PASS " ++ pass
    writeMessage' (BT.ClientNick nick) =
        "NICK " ++ getNickname nick
    writeMessage' (BT.ClientUser user mode realname) =
        "USER " ++ user ++ " " ++ show mode ++ " * :" ++ realname
    writeMessage' (BT.ClientOper user pass) =
        "OPER " ++ user ++ " " ++ pass
    writeMessage' (BT.ClientMode nick mode) =
        "MODE " ++ getNickname nick ++ " " ++ mode
    writeMessage' (BT.ClientQuit reason) =
        "QUIT :" ++ reason
    writeMessage' (BT.ClientJoin channelsAndKeys) =
        let (channels, keys) = unzip channelsAndKeys
        in "JOIN " ++ intercalate "," (map getChannel channels) ++ " "
            ++ intercalate "," keys
    writeMessage' (BT.ClientPart channels (Just message)) =
        "PART " ++ intercalate "," (map getChannel channels) ++ " " ++ message
    writeMessage' (BT.ClientPart channels Nothing) =
        "PART " ++ intercalate "," (map getChannel channels) ++ " "
    writeMessage' (BT.ClientTopic chan (Just topic)) =
        "TOPIC " ++ getChannel chan ++ " :" ++ topic
    writeMessage' (BT.ClientTopic chan Nothing) =
        "TOPIC " ++ getChannel chan
    writeMessage' (BT.ClientNames channels) =
        "NAMES " ++ intercalate "," (map getChannel channels)
    writeMessage' (BT.ClientList channels) =
        "LIST " ++ intercalate "," (map getChannel channels)
    writeMessage' (BT.ClientInvite nick chan) =
        "INVITE " ++ getNickname nick ++ " " ++ getChannel chan
    writeMessage' (BT.ClientPrivMsg user message) =
        "PRIVMSG " ++ writeUser user ++ " :" ++ message
    writeMessage' (BT.ClientNotice user message) =
        "NOTICE " ++ writeUser user ++ " :" ++ message
    writeMessage' (BT.ClientWho mask) =
        "WHO " ++ mask
    writeMessage' (BT.ClientWhoIs (Just server) user) =
        "WHOIS " ++ getServerName server ++ " " ++ user
    writeMessage' (BT.ClientWhoIs Nothing user) =
        "WHOIS " ++ user
    writeMessage' (BT.ClientWhoWas user Nothing Nothing) =
        "WHOWAS " ++ user
    writeMessage' (BT.ClientWhoWas user (Just size) Nothing) =
        "WHOWAS " ++ user ++ " " ++ show size
    writeMessage' (BT.ClientWhoWas user (Just size) (Just server)) =
        "WHOWAS " ++ user ++ " " ++ show size ++ " " ++ getServerName server
    writeMessage' (BT.ClientPing servername) =
        "PING " ++ getServerName servername
    writeMessage' (BT.ClientPong servername) =
        "PONG " ++ getServerName servername
    writeMessage' (BT.ClientPrivMsgChan chan message) =
        "PRIVMSG " ++ getChannel chan ++ " :" ++ message

writeServerMessage :: BT.ServerMessage -> String
writeServerMessage (BT.ServerNick user newnick) =
    ":" ++ writeUser user ++ " NICK " ++ getNickname newnick ++ "\r\n"
writeServerMessage (BT.ServerJoin user chan) =
    ":" ++ writeUser user ++ " JOIN " ++ getChannel chan ++ "\r\n"
writeServerMessage (BT.ServerPart user chan message) =
    ":" ++ writeUser user ++ " PRIVMSG " ++ getChannel chan ++ " :" ++ message
        ++ "\r\n"
writeServerMessage (BT.ServerQuit user reason) =
    ":" ++ writeUser user ++ " QUIT :" ++ reason ++ "\r\n"
writeServerMessage (BT.ServerTopic user chan newtopic) =
    ":" ++ writeUser user ++ " TOPIC " ++ getChannel chan ++ " :" ++ newtopic
        ++ "\r\n"
writeServerMessage (BT.ServerInvite user nick chan) =
    ":" ++ writeUser user ++ " INVITE " ++ getNickname nick ++ " "
        ++ getChannel chan ++ "\r\n"
writeServerMessage (BT.ServerPrivMsg user targets message) =
    ":" ++ writeUser user ++ " PRIVMSG " ++ writeTargets targets ++ " :"
        ++ message ++ "\r\n"
writeServerMessage (BT.ServerNotice server targets message) =
    ":" ++ getServerName server ++ " NOTICE " ++ writeTargets targets ++ " :"
        ++ message ++ "\r\n"
writeServerMessage (BT.ServerPing server) =
    ":" ++ getServerName server ++ " PING\r\n"
writeServerMessage (BT.ServerKick user chan nick) =
    ":" ++ writeUser user ++ " KICK " ++ getChannel chan ++ " "
        ++ getNickname nick ++ "\r\n"
writeServerMessage (BT.ServerMode user nick mode) =
    ":" ++ writeUser user ++ " MODE " ++ getNickname nick ++ " :" ++ mode
        ++ "\r\n"
writeServerMessage (BT.ServerReply server numcom []) =
    ":" ++ getServerName server ++ " " ++ show numcom ++ "\r\n"
writeServerMessage (BT.ServerReply server numcom [arg]) =
    ":" ++ getServerName server ++ " " ++ show numcom ++ " :" ++ arg ++ "\r\n"
writeServerMessage (BT.ServerReply server numcom args) =
    ":" ++ getServerName server ++ " " ++ show numcom ++ " "
        ++ unwords (init args) ++ " :" ++ last args ++ "\r\n"

writeUser :: BT.IRCUser -> String
writeUser (BT.IRCUser nick Nothing Nothing) =
    getNickname nick
writeUser (BT.IRCUser nick Nothing (Just host)) =
    getNickname nick ++ "@" ++ host
writeUser (BT.IRCUser nick (Just user) Nothing) =
    getNickname nick ++ "!" ++ user
writeUser (BT.IRCUser nick (Just user) (Just host)) =
    getNickname nick ++ "!" ++ user ++ "@" ++ host

writeTargets :: [BT.Target] -> String
writeTargets targets = intercalate "," (map writeTarget targets)

writeTarget :: BT.Target -> String
writeTarget (BT.ChannelTarget chan) = getChannel chan
writeTarget (BT.UserTarget server) = writeUserServer server
writeTarget (BT.NickTarget user) = writeUser user

writeUserServer :: BT.UserServer -> String
writeUserServer (BT.UserServer user Nothing Nothing) = user
writeUserServer (BT.UserServer user Nothing (Just server)) =
    user ++ "@" ++ getServerName server
writeUserServer (BT.UserServer user (Just host) Nothing) = user ++ "%" ++ host
writeUserServer (BT.UserServer user (Just host) (Just server)) =
    user ++ "%" ++ host ++ "@" ++ getServerName server
