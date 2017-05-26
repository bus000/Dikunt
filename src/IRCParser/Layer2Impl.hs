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

import qualified Data.Text.Lazy as T
import BotTypes
    ( IRCMessage(..)
    , ServerMessage(..)
    , IRCCommand(..)
    , IRCPrefix(..)
    )

parseServerMessage :: IRCMessage -> Maybe ServerMessage
parseServerMessage (IRCMessage
    (Just (NicknamePrefix usr)) NICK [nick] Nothing) =
        Just $ ServerNick usr nick

parseServerMessage (IRCMessage
    (Just (NicknamePrefix usr)) JOIN [chan] Nothing) =
        Just $ ServerJoin usr chan

parseServerMessage (IRCMessage
    (Just (NicknamePrefix usr)) PART [chan] (Just msg)) =
        Just $ ServerPart usr chan msg

parseServerMessage (IRCMessage
    (Just (NicknamePrefix usr)) TOPIC [chan] (Just topic)) =
        Just $ ServerTopic usr chan topic

parseServerMessage (IRCMessage
    (Just (NicknamePrefix usr)) INVITE [nick, chan] Nothing) =
        Just $ ServerInvite usr nick chan

parseServerMessage (IRCMessage
    (Just (NicknamePrefix usr)) PRIVMSG [nick] (Just msg)) =
        Just $ ServerPrivMsg usr nick msg

parseServerMessage
    (IRCMessage (Just (NicknamePrefix usr)) NOTICE [nick] (Just msg)) =
        Just $ ServerNotice usr nick msg

parseServerMessage (IRCMessage Nothing PING [] (Just servername)) =
    Just $ ServerPing (T.unpack servername)

parseServerMessage (IRCMessage (Just (NicknamePrefix usr)) QUIT [] (Just msg)) =
    Just $ ServerQuit usr msg

parseServerMessage (IRCMessage
    (Just (ServernamePrefix server)) (NUMCOM n) params msg) =
        Just $ ServerReply server n params msg

parseServerMessage _ = Nothing
