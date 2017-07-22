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

import BotTypes (ClientMessage(..), IRCUser(..), ServerMessage(..), getNickname)
import Data.List (intercalate)

{- | Convert client messages to strings that can be send to the IRC server. -}
writeMessage :: ClientMessage
    -- ^ The message to convert to a string.
    -> String
writeMessage msg = writeMessage' msg ++ "\r\n"
  where
    writeMessage' (ClientPass pass) =
        "PASS " ++ pass
    writeMessage' (ClientNick nick) =
        "NICK " ++ getNickname nick
    writeMessage' (ClientUser user mode realname) =
        "USER " ++ user ++ " " ++ show mode ++ " * :" ++ realname
    writeMessage' (ClientOper user pass) =
        "OPER " ++ user ++ " " ++ pass
    writeMessage' (ClientMode nick mode) =
        "MODE " ++ getNickname nick ++ " " ++ mode
    writeMessage' (ClientQuit reason) =
        "QUIT :" ++ reason
    writeMessage' (ClientJoin channelsAndKeys) =
        let (channels, keys) = unzip channelsAndKeys
        in "JOIN " ++ intercalate "," channels ++ " " ++ intercalate "," keys
    writeMessage' (ClientPart channels (Just message)) =
        "PART " ++ intercalate "," channels ++ " " ++ message
    writeMessage' (ClientPart channels Nothing) =
        "PART " ++ intercalate "," channels ++ " "
    writeMessage' (ClientTopic chan (Just topic)) =
        "TOPIC " ++ chan ++ " :" ++ topic
    writeMessage' (ClientTopic chan Nothing) =
        "TOPIC " ++ chan
    writeMessage' (ClientNames channels) =
        "NAMES " ++ intercalate "," channels
    writeMessage' (ClientList channels) =
        "LIST " ++ intercalate "," channels
    writeMessage' (ClientInvite nick chan) =
        "INVITE " ++ getNickname nick ++ " " ++ chan
    writeMessage' (ClientPrivMsg user message) =
        "PRIVMSG " ++ writeUser user ++ " :" ++ message
    writeMessage' (ClientNotice user message) =
        "NOTICE " ++ writeUser user ++ " :" ++ message
    writeMessage' (ClientWho mask) =
        "WHO " ++ mask
    writeMessage' (ClientWhoIs (Just server) user) =
        "WHOIS " ++ server ++ " " ++ user
    writeMessage' (ClientWhoIs Nothing user) =
        "WHOIS " ++ user
    writeMessage' (ClientWhoWas user Nothing Nothing) =
        "WHOWAS " ++ user
    writeMessage' (ClientWhoWas user (Just size) Nothing) =
        "WHOWAS " ++ user ++ " " ++ show size
    writeMessage' (ClientWhoWas user (Just size) (Just server)) =
        "WHOWAS " ++ user ++ " " ++ show size ++ " " ++ server
    writeMessage' (ClientPing servername) =
        "PING " ++ servername
    writeMessage' (ClientPong servername) =
        "PONG " ++ servername
    writeMessage' (ClientPrivMsgChan chan message) =
        "PRIVMSG " ++ chan ++ " :" ++ message

writeServerMessage :: ServerMessage -> String
writeServerMessage (ServerNick user newnick) =
    ":" ++ writeUser user ++ " NICK " ++ getNickname newnick ++ "\r\n"
writeServerMessage (ServerJoin user chan) =
    ":" ++ writeUser user ++ " JOIN " ++ chan ++ "\r\n"
writeServerMessage (ServerPart user chan message) =
    ":" ++ writeUser user ++ " PRIVMSG " ++ chan ++ " :" ++ message ++ "\r\n"
writeServerMessage (ServerQuit user reason) =
    ":" ++ writeUser user ++ " QUIT :" ++ reason ++ "\r\n"
writeServerMessage (ServerTopic user chan newtopic) =
    ":" ++ writeUser user ++ " TOPIC " ++ chan ++ " :" ++ newtopic ++ "\r\n"
writeServerMessage (ServerInvite user nick chan) =
    ":" ++ writeUser user ++ " INVITE " ++ getNickname nick ++ " " ++ chan ++ "\r\n"
writeServerMessage (ServerPrivMsg user chan message) =
    ":" ++ writeUser user ++ " PRIVMSG " ++ chan ++ " :" ++ message ++ "\r\n"
writeServerMessage (ServerNotice user chan message) =
    ":" ++ writeUser user ++ " NOTICE " ++ chan ++ " :" ++ message ++ "\r\n"
writeServerMessage (ServerNoticeServer servername chan message) =
    ":" ++ servername ++ " NOTICE " ++ chan ++ " :" ++ message ++ "\r\n"
writeServerMessage (ServerPing servername) =
    ":" ++ servername ++ " PING\r\n"
writeServerMessage (ServerReply servername numcom args (Just trailing)) =
    ":" ++ servername ++ " " ++ show numcom ++ " " ++ intercalate " " args
        ++ " :" ++ trailing ++ "\r\n"
writeServerMessage (ServerReply servername numcom args Nothing) =
    ":" ++ servername ++ " " ++ show numcom ++ " " ++ intercalate " " args
        ++ "\r\n"

writeUser :: IRCUser -> String
writeUser (IRCUser nick Nothing Nothing) =
    getNickname nick
writeUser (IRCUser nick Nothing (Just host)) =
    getNickname nick ++ "@" ++ host
writeUser (IRCUser nick (Just user) Nothing) =
    getNickname nick ++ "!" ++ user
writeUser (IRCUser nick (Just user) (Just host)) =
    getNickname nick ++ "!" ++ user ++ "@" ++ host
