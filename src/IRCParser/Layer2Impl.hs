{- |
 - Module      : IRCParser.Layer2Impl
 - Description : Parses IRC messages.
 - Copyright   : (c) Magnus Stavngaard, 2016
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - Implementation of parsing IRCMessage's to Message's which is a further
 - abstraction of IRC messages.
 -}
module IRCParser.Layer2Impl where

import BotTypes
    ( IRCMessage(..)
    , ServerMessage(..)
    , IRCCommand(..)
    , IRCPrefix(..)
    , nickname
    )

parseSM :: IRCMessage -> Maybe ServerMessage
parseSM (IRCMessage (Just (NicknamePrefix usr)) NICK [nick] Nothing)
    | Just n <- nickname nick = Just $ ServerNick usr n

parseSM (IRCMessage (Just (NicknamePrefix usr)) JOIN [chan] Nothing) =
    Just $ ServerJoin usr chan

parseSM (IRCMessage (Just (NicknamePrefix usr)) PART [chan] (Just msg)) =
    Just $ ServerPart usr chan msg

parseSM (IRCMessage (Just (NicknamePrefix usr)) TOPIC [chan] (Just topic)) =
    Just $ ServerTopic usr chan topic

parseSM (IRCMessage (Just (NicknamePrefix usr)) INVITE [nick, chan] Nothing)
    | Just n <- nickname nick = Just $ ServerInvite usr n chan

parseSM (IRCMessage (Just (NicknamePrefix usr)) PRIVMSG [chan] (Just msg)) =
    Just $ ServerPrivMsg usr chan msg

parseSM (IRCMessage (Just (NicknamePrefix usr)) NOTICE [chan] (Just msg)) =
    Just $ ServerNotice usr chan msg

parseSM (IRCMessage (Just (ServernamePrefix servername)) NOTICE [chan] (Just msg)) =
    Just $ ServerNoticeServer servername chan msg

parseSM (IRCMessage Nothing PING [] (Just servername)) =
    Just $ ServerPing servername

parseSM (IRCMessage (Just (NicknamePrefix usr)) QUIT [] (Just msg)) =
    Just $ ServerQuit usr msg

parseSM (IRCMessage (Just (ServernamePrefix server)) (NUMCOM n) params msg) =
    Just $ ServerReply server n params msg

parseSM _ = Nothing
