{- |
 - Module      : Parsers.IRCMessageParser
 - Description : Parses IRC messages.
 - Copyright   : (c) Magnus Stavngaard, 2016
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - Parse IRC messages from a string to an IRCMessage the general structure of an
 - IRC message is
 -
 -     message = [ ":" prefix SPACE ] command [ params ] crlf.
 -
 - See https://tools.ietf.org/html/rfc2812 for details.
 -}
module Parsers.IRCMessageParser ( parseMessage ) where

import qualified Data.Text.Lazy as T
import qualified Parsers.Utils as PU
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Types.BotTypes as BT
import qualified Types.Internal.Channel as BT
import qualified Types.Internal.Hostname as BT
import qualified Types.Internal.Message as BT
import qualified Types.Internal.Nickname as BT
import qualified Types.Internal.ServerMessage as BT
import qualified Types.Internal.Servername as BT
import qualified Types.Internal.Target as BT
import qualified Types.Internal.Username as BT

type BotParser a = P.Parsec T.Text () a

{- | Parse a message from the IRC server to the internal structure. -}
parseMessage :: T.Text
    -- ^ Source to parse from.
    -> Either P.ParseError BT.ServerMessage
parseMessage = P.parse servermessage "(IRC message)"

servermessage :: BotParser BT.ServerMessage
servermessage = P.choice messages <* P.crlf <* P.eof
  where
    messages = map P.try
        [ nickMessage
        , quitMessage
        , joinMessage
        , partMessage
        , topicMessage
        , inviteMessage
        , kickMessage
        , privmsgMessage
        , noticeMessage
        , pingMessage
        , replyMessage
        , modeMessage
        ]

nickMessage :: BotParser BT.ServerMessage
nickMessage = BT.ServerNick <$> prefix nickUserHost <* P.string "NICK " <*>
    nickname

quitMessage :: BotParser BT.ServerMessage
quitMessage = BT.ServerQuit <$> prefix nickUserHost <* P.string "QUIT " <*>
    message

joinMessage :: BotParser BT.ServerMessage
joinMessage = BT.ServerJoin <$> prefix nickUserHost <* P.string "JOIN " <*>
    channel

partMessage :: BotParser BT.ServerMessage
partMessage = BT.ServerPart <$> prefix nickUserHost <* P.string "PART " <*>
    channel <* P.char ' ' <*> message

topicMessage :: BotParser BT.ServerMessage
topicMessage = BT.ServerTopic <$> prefix nickUserHost <* P.string "TOPIC " <*>
    channel <* P.char ' ' <*> message

inviteMessage :: BotParser BT.ServerMessage
inviteMessage = BT.ServerInvite <$> prefix nickUserHost <* P.string "INVITE "
    <*> nickname <* P.char ' ' <*> channel

kickMessage :: BotParser BT.ServerMessage
kickMessage = BT.ServerKick <$> prefix nickUserHost <* P.string "KICK " <*>
    channel <* P.char ' ' <*> nickname

privmsgMessage :: BotParser BT.ServerMessage
privmsgMessage = BT.ServerPrivMsg <$> prefix nickUserHost <* P.string "PRIVMSG "
    <*> targets <* P.char ' ' <*> message

noticeMessage :: BotParser BT.ServerMessage
noticeMessage = BT.ServerNotice <$> prefix servername <* P.string "NOTICE " <*>
    targets <* P.char ' ' <*> message

pingMessage :: BotParser BT.ServerMessage
pingMessage = BT.ServerPing <$> (P.string "PING :" *> servername)

modeMessage :: BotParser BT.ServerMessage
modeMessage = BT.ServerMode <$> prefix nickUserHost <* P.string "MODE " <*>
    nickname <* P.char ' ' <*> message

replyMessage :: BotParser BT.ServerMessage
replyMessage = BT.ServerReply <$> prefix servername <*> P.decimal <*
    P.char ' ' <*> args

args :: BotParser BT.Arguments
args = BT.Arguments <$> (P.try noTrailing <|> P.try withTrailing <|>
    P.try onlyTrailing)
  where
    noTrailing = P.sepBy1 arg space
    withTrailing = (\xs x -> xs ++ [x]) <$> P.endBy arg space <*> trailing
    onlyTrailing = (: []) <$> trailing
    arg = (:) <$> P.noneOf " \0\r\n:" <*> P.many (P.noneOf " \0\r\n")
    space = P.char ' '

message :: BotParser BT.Message
message = BT.Message <$> trailing

trailing :: BotParser String
trailing = P.char ':' >> P.many1 (P.noneOf "\0\r\n")

nickname :: BotParser BT.Nickname
nickname = BT.Nickname <$> PU.nickname

servername :: BotParser BT.Servername
servername = BT.Servername <$> (P.try nickservHost <|>
    (BT.getHostname <$> P.try hostnameAddress))

channel :: BotParser BT.Channel
channel = BT.Channel <$> PU.channel

targets :: BotParser BT.Targets
targets = BT.Targets <$> P.sepBy1 target (P.char ',')

target :: BotParser BT.Target
target = P.choice
    [ BT.ChannelTarget <$> P.try channel
    , BT.NickTarget <$> P.try forceNickUserNoHost
    , BT.UserTarget <$> P.try forceUserHostNoServer
    , BT.NickTarget <$> P.try nickUserHost
    , BT.UserTarget <$> P.try userHostServer
    ]

nickUserHost :: BotParser BT.IRCUser
nickUserHost = BT.IRCUser <$> nickname <*> user <*> host
  where
    user = P.optionMaybe $ P.char '!' *> username
    host = P.optionMaybe $ P.char '@' *> hostnameAddress

forceNickUserNoHost :: BotParser BT.IRCUser
forceNickUserNoHost = BT.IRCUser <$> nickname <*> user <*> host
  where
    user = Just <$> (P.char '!' *> username)
    host = P.optionMaybe $ P.char '@' *> hostnameAddress

userHostServer :: BotParser BT.UserServer
userHostServer = BT.UserServer <$> username <*> host <*> server
  where
    host = P.optionMaybe $ P.char '%' *> hostnameAddress
    server = P.optionMaybe $ P.char '@' *> servername

forceUserHostNoServer :: BotParser BT.UserServer
forceUserHostNoServer = BT.UserServer <$> username <*> host <*> server
  where
    host = Just <$> (P.char '%' *> hostnameAddress)
    server = P.optionMaybe $ P.char '@' *> servername

prefix :: BotParser a -> P.Parsec T.Text () a
prefix = P.between (P.char ':') (P.char ' ')

username :: BotParser BT.Username
username = BT.Username <$> PU.username

hostnameAddress :: BotParser BT.Hostname
hostnameAddress = BT.Hostname <$> (P.try hostAddress <|> P.try PU.hostname)

nickservHost :: BotParser String
nickservHost = P.string "NickServ!NickServ@services."

hostAddress :: BotParser String
hostAddress = P.try PU.ipV4 <|> P.try PU.ipV6
