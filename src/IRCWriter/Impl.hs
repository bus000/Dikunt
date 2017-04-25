{- TODO: Add module header. -}
module IRCWriter.Impl where

import BotTypes
    ( ClientMessage(..)
    , IRCUser(..)
    )
import Data.List (intercalate)

writeMessage :: ClientMessage -> String
writeMessage msg = writeMessage' msg ++ "\r\n"
  where
    writeMessage' (ClientPass pass) =
        "PASS " ++ pass

    writeMessage' (ClientNick nick) =
        "NICK " ++ nick

    writeMessage' (ClientUser user mode realname) =
        "USER " ++ user ++ " " ++ show mode ++ " * :" ++ realname

    writeMessage' (ClientOper user pass) =
        "OPER " ++ user ++ " " ++ pass

    writeMessage' (ClientMode nick mode) =
        "MODE " ++ nick ++ " " ++ mode

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

    writeMessage' (ClientInvite chan nick) =
        "INVITE " ++ nick ++ " " ++ chan

    writeMessage' (ClientPrivMsg (IRCUser nick Nothing Nothing) message) =
        "PRIVMSG " ++ nick ++ " :" ++ message

    writeMessage' (ClientPrivMsg (IRCUser nick (Just user) Nothing) message) =
        "PRIVMSG " ++ nick ++ "!" ++ user ++ " :" ++ message

    writeMessage' (ClientPrivMsg (IRCUser nick Nothing (Just host)) message) =
        "PRIVMSG " ++ nick ++ "@" ++ host ++ " :" ++ message

    writeMessage' (ClientPrivMsg (IRCUser nick (Just user) (Just host)) message) =
        "PRIVMSG " ++ nick ++ "!" ++ user ++ "@" ++ host ++ " :" ++ message

    writeMessage' (ClientNotice (IRCUser nick Nothing Nothing) message) =
        "NOTICE " ++ nick ++ " :" ++ message

    writeMessage' (ClientNotice (IRCUser nick (Just user) Nothing) message) =
        "NOTICE " ++ nick ++ "!" ++ user ++ " :" ++ message

    writeMessage' (ClientNotice (IRCUser nick Nothing (Just host)) message) =
        "NOTICE " ++ nick ++ "@" ++ host ++ " :" ++ message

    writeMessage' (ClientNotice (IRCUser nick (Just user) (Just host)) message) =
        "NOTICE " ++ nick ++ "!" ++ user ++ "@" ++ host ++ " :" ++ message

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
