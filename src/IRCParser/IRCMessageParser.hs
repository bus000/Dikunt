{- |
 - Module      : IRCParser.Parser
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
module IRCParser.IRCMessageParser ( parseMessage ) where

import qualified BotTypes as BT
import qualified Data.Char as Char
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T
import Numeric (showHex)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

parseMessage :: T.Text -> Either P.ParseError BT.ServerMessage
parseMessage = P.parse servermessage "(IRC message)"

servermessage :: P.Parsec T.Text () BT.ServerMessage
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

nickMessage :: P.Parsec T.Text () BT.ServerMessage
nickMessage = BT.ServerNick <$> prefix nickUserHost <* P.string "NICK " <*>
    nickname

quitMessage :: P.Parsec T.Text () BT.ServerMessage
quitMessage = BT.ServerQuit <$> prefix nickUserHost <* P.string "QUIT " <*>
    trailing

joinMessage :: P.Parsec T.Text () BT.ServerMessage
joinMessage = BT.ServerJoin <$> prefix nickUserHost <* P.string "JOIN " <*>
    channel

partMessage :: P.Parsec T.Text () BT.ServerMessage
partMessage = BT.ServerPart <$> prefix nickUserHost <* P.string "PART " <*>
    channel <* P.char ' ' <*> trailing

topicMessage :: P.Parsec T.Text () BT.ServerMessage
topicMessage = BT.ServerTopic <$> prefix nickUserHost <* P.string "TOPIC " <*>
    channel <* P.char ' ' <*> trailing

inviteMessage :: P.Parsec T.Text () BT.ServerMessage
inviteMessage = BT.ServerInvite <$> prefix nickUserHost <* P.string "INVITE "
    <*> nickname <* P.char ' ' <*> channel

kickMessage :: P.Parsec T.Text () BT.ServerMessage
kickMessage = BT.ServerKick <$> prefix nickUserHost <* P.string "KICK " <*>
    channel <* P.char ' ' <*> nickname

privmsgMessage :: P.Parsec T.Text () BT.ServerMessage
privmsgMessage = BT.ServerPrivMsg <$> prefix nickUserHost <* P.string "PRIVMSG "
    <*> targets <* P.char ' ' <*> trailing

noticeMessage :: P.Parsec T.Text () BT.ServerMessage
noticeMessage = BT.ServerNotice <$> prefix servername <* P.string "NOTICE " <*>
    targets <* P.char ' ' <*> trailing

pingMessage :: P.Parsec T.Text () BT.ServerMessage
pingMessage = BT.ServerPing <$> (P.string "PING :" *> servername)

modeMessage :: P.Parsec T.Text () BT.ServerMessage
modeMessage = BT.ServerMode <$> prefix nickUserHost <* P.string "MODE " <*>
    nickname <* P.char ' ' <*> trailing

replyMessage :: P.Parsec T.Text () BT.ServerMessage
replyMessage = BT.ServerReply <$> prefix servername <*> P.decimal <* P.char ' '
    <*> args

trailing :: P.Parsec T.Text () String
trailing = P.char ':' >> P.many (P.noneOf "\0\r\n")

nickname :: P.Parsec T.Text () BT.Nickname
nickname =
    consNick <$> P.alphaNum <*> P.many (P.choice [P.alphaNum, P.char '-', special])
  where
    consNick c1 str = fromMaybe (error "The parsed nickname should be correct.")
        (BT.nickname (c1:str))

servername :: P.Parsec T.Text () BT.Servername
servername = P.try nickservHost <|> P.try hostname

channel :: P.Parsec T.Text () BT.Channel
channel = consChan <$> P.choice [P.char '#', P.char '+', P.char '&'] <*>
    P.many1 (P.noneOf "\0\a\r\n ,:")
  where
    consChan c1 str = fromMaybe (error "The parsed channel should be correct.")
        (BT.channel (c1:str))

targets :: P.Parsec T.Text () [BT.Target]
targets = P.sepBy1 target (P.char ',')

target :: P.Parsec T.Text () BT.Target
target = P.choice
    [ BT.ChannelTarget <$> P.try channel
    , BT.NickTarget <$> P.try forceNickUserNoHost
    , BT.UserTarget <$> P.try forceUserHostNoServer
    , BT.NickTarget <$> P.try nickUserHost
    , BT.UserTarget <$> P.try userHostServer
    ]

nickUserHost :: P.Parsec T.Text () BT.IRCUser
nickUserHost = BT.IRCUser <$> nickname <*> user <*> host
  where
    user = P.optionMaybe $ P.char '!' *> username
    host = P.optionMaybe $ P.char '@' *> hostname

forceNickUserNoHost :: P.Parsec T.Text () BT.IRCUser
forceNickUserNoHost = BT.IRCUser <$> nickname <*> user <*> host
  where
    user = Just <$> (P.char '!' *> username)
    host = P.optionMaybe $ P.char '@' *> hostname

userHostServer :: P.Parsec T.Text () BT.UserServer
userHostServer = BT.UserServer <$> username <*> host <*> server
  where
    host = P.optionMaybe $ P.char '%' *> hostname
    server = P.optionMaybe $ P.char '@' *> servername

forceUserHostNoServer :: P.Parsec T.Text () BT.UserServer
forceUserHostNoServer = BT.UserServer <$> username <*> host <*> server
  where
    host = Just <$> (P.char '%' *> hostname)
    server = P.optionMaybe $ P.char '@' *> servername

special :: P.Parsec T.Text () Char
special = P.choice $ map P.char ['[', ']', '\\', '`', '_', '^', '{', '}', '|']

args :: P.Parsec T.Text () [String]
args = P.sepBy arg (P.char ' ')

arg :: P.Parsec T.Text () String
arg = P.many (P.noneOf "\0\r\n ")

shortname :: P.Parsec T.Text () String
shortname = do
    short <- P.many1 (P.choice [P.alphaNum, P.char '-'])
    if Char.isAlphaNum (head short) && Char.isAlphaNum (last short)
    then return short
    else P.parserFail "Shortname cannot start or end with '-'"

prefix :: P.Parsec T.Text () a -> P.Parsec T.Text () a
prefix = P.between (P.char ':') (P.char ' ')

username :: P.Parsec T.Text () BT.Username
username = P.many1 (P.noneOf "\0\r\n @%")

hostname :: P.Parsec T.Text () BT.Hostname
hostname =  P.try hostAddress <|> P.try hostname'

hostname' :: P.Parsec T.Text () BT.Hostname
hostname' = intercalate "." <$> P.sepBy1 shortname (P.char '.')

nickservHost :: P.Parsec T.Text () BT.Hostname
nickservHost = P.string "NickServ!NickServ@services."

hostAddress :: P.Parsec T.Text () BT.Hostname
hostAddress = (ipV4ToString <$> P.try ipV4) <|> (ipV6ToString <$> P.try ipV6)

ipV4 :: P.Parsec T.Text () IPV4
ipV4 = IPV4 <$> (P.decimal <* period) <*> (P.decimal <* period)
    <*> (P.decimal <* period) <*> P.decimal
  where
    period = P.char '.'

ipV6 :: P.Parsec T.Text () IPV6
ipV6 = do
    cons <- P.try withBridge <|> P.try noBridge
    case ipV6Cons cons of
        Just a -> return a
        Nothing -> P.parserFail "Could not parse IPV6."
  where
    withBridge :: P.Parsec T.Text () [IPV6Con]
    withBridge = (\x y -> x ++ [Zeros] ++ y) <$>
        P.endBy1 (Num <$> P.hexnum) (P.char ':') <* P.char ':' <*> noBridge
    noBridge = P.sepBy1 (Num <$> P.hexnum) (P.char ':')

data IPV4 = IPV4 Int Int Int Int deriving (Show)

ipV4ToString :: IPV4 -> String
ipV4ToString (IPV4 a b c d) = intercalate "." $ map show [a, b, c, d]

data IPV6 = IPV6 Int Int Int Int Int Int Int Int deriving (Show)

data IPV6Con = Num Int | Zeros deriving (Eq, Show)

ipV6Cons :: [IPV6Con] -> Maybe IPV6
ipV6Cons cons
    | bridges == 0 && length cons == 8 = fromList $ map (\(Num n) -> n) cons
    | bridges == 1 = fromList $ foldr expand [] cons
    | otherwise = Nothing
  where
    bridges = length . filter (== Zeros) $ cons
    bridgeLen = 8 - length cons + 1

    fromList [a, b, c, d, e, f, g, h] = Just $ IPV6 a b c d e f g h
    fromList _ = Nothing

    expand Zeros ls = replicate bridgeLen 0 ++ ls
    expand (Num n) ls = n:ls

ipV6ToString :: IPV6 -> String
ipV6ToString (IPV6 a b c d e f g h) =
    intercalate ":" $ map (`showHex` "") [a, b, c, d, e, f, g, h]
