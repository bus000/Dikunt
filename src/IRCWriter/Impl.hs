{- TODO: Add module header. -}
{-# LANGUAGE OverloadedStrings #-}
module IRCWriter.Impl where

import BotTypes
    ( ClientMessage(..)
    , IRCUser(..)
    )
import Data.List (intercalate)
import qualified Data.Text.Lazy as T

writeMessage :: ClientMessage -> T.Text
writeMessage msg = T.concat [writeMessage' msg, "\r\n"]
  where
    writeMessage' :: ClientMessage -> T.Text
    writeMessage' (ClientPass pass) =
        T.concat ["PASS ", T.pack pass]

    writeMessage' (ClientNick nick) =
        T.concat ["NICK ", T.pack nick]

    writeMessage' (ClientUser user mode realname) =
        T.concat ["USER ", T.pack user, " ", T.pack . show $ mode, " * :",
            T.pack realname]

    writeMessage' (ClientOper user pass) =
        T.concat ["OPER ", T.pack user, " ", T.pack pass]

    writeMessage' (ClientMode nick mode) =
        T.concat ["MODE ", T.pack nick, " ", T.pack mode]

    writeMessage' (ClientQuit reason) =
        T.concat ["QUIT :", reason]

    writeMessage' (ClientJoin channelsAndKeys) =
        let (channels, keys) = unzip channelsAndKeys
        in T.concat ["JOIN ", T.pack $ intercalate "," channels, " ",
            T.pack $ intercalate "," keys]

    writeMessage' (ClientPart channels (Just message)) =
        T.concat ["PART ", T.pack $ intercalate "," channels, " ", message]

    writeMessage' (ClientPart channels Nothing) =
        T.concat ["PART ", T.pack $ intercalate "," channels]

    writeMessage' (ClientTopic chan (Just topic)) =
        T.concat ["TOPIC ", T.pack chan, " :", topic]

    writeMessage' (ClientTopic chan Nothing) =
        T.concat ["TOPIC ", T.pack chan]

    writeMessage' (ClientNames channels) =
        T.concat ["NAMES ", T.pack $ intercalate "," channels]

    writeMessage' (ClientList channels) =
        T.concat ["LIST ", T.pack $ intercalate "," channels]

    writeMessage' (ClientInvite chan nick) =
        T.concat ["INVITE ", T.pack nick, " ", T.pack chan]

    writeMessage' (ClientPrivMsg (IRCUser nick Nothing Nothing) message) =
        T.concat ["PRIVMSG ", T.pack nick, " :", message]

    writeMessage' (ClientPrivMsg (IRCUser nick (Just user) Nothing) message) =
        T.concat ["PRIVMSG ", T.pack nick, "!", T.pack user, " :", message]

    writeMessage' (ClientPrivMsg (IRCUser nick Nothing (Just host)) message) =
        T.concat ["PRIVMSG ", T.pack nick, "@", T.pack host, " :", message]

    writeMessage' (ClientPrivMsg (IRCUser nick (Just user) (Just host)) message) =
        T.concat ["PRIVMSG ", T.pack nick, "!", T.pack user, "@", T.pack host,
            " :", message]

    writeMessage' (ClientNotice (IRCUser nick Nothing Nothing) message) =
        T.concat ["NOTICE ", T.pack nick, " :", message]

    writeMessage' (ClientNotice (IRCUser nick (Just user) Nothing) message) =
        T.concat ["NOTICE ", T.pack nick, "!", T.pack user, " :", message]

    writeMessage' (ClientNotice (IRCUser nick Nothing (Just host)) message) =
        T.concat ["NOTICE ", T.pack nick, "@", T.pack host, " :", message]

    writeMessage' (ClientNotice (IRCUser nick (Just user) (Just host)) message) =
        T.concat ["NOTICE ", T.pack nick, "!", T.pack user, "@", T.pack host,
            " :", message]

    writeMessage' (ClientWho mask) =
        T.concat ["WHO ", T.pack mask]

    writeMessage' (ClientWhoIs (Just server) user) =
        T.concat ["WHOIS ", T.pack server, " ", T.pack user]

    writeMessage' (ClientWhoIs Nothing user) =
        T.concat ["WHOIS ", T.pack user]

    writeMessage' (ClientWhoWas user Nothing Nothing) =
        T.concat ["WHOWAS ", T.pack user]

    writeMessage' (ClientWhoWas user (Just size) Nothing) =
        T.concat ["WHOWAS ", T.pack user, " ", T.pack $ show size]

    writeMessage' (ClientWhoWas user (Just size) (Just server)) =
        T.concat ["WHOWAS ", T.pack user, " ", T.pack $ show size, " ",
            T.pack server]

    writeMessage' (ClientPing servername) =
        T.concat ["PING ", T.pack servername]

    writeMessage' (ClientPong servername) =
        T.concat ["PONG ", T.pack servername]

    writeMessage' (ClientPrivMsgChan chan message) =
        T.concat ["PRIVMSG ", T.pack chan, " :", message]
