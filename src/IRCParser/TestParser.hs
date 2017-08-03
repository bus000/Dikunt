module IRCParser.TestParser where

import qualified BotTypes as BT
import qualified Data.Char as Char
import Data.List (intercalate)
import qualified Data.Text.Lazy as T
import Numeric (showHex)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

data IPV4 = IPV4 Int Int Int Int deriving (Show)

ipV4ToString :: IPV4 -> String
ipV4ToString (IPV4 a b c d) = intercalate "." $ map show [a, b, c, d]

data IPV6 = IPV6 Int Int Int Int Int Int Int Int deriving (Show)

ipV6ToString :: IPV6 -> String
ipV6ToString (IPV6 a b c d e f g h) =
    intercalate ":" $ map (`showHex` "") [a, b, c, d, e, f, g, h]

data ServerMessage
    -- | ServerNick <user> <nick> - user change nickname to nick.
    = ServerNick BT.IRCUser BT.Nickname
    -- | ServerQuit <user> <message> - user quit IRC.
    | ServerQuit BT.IRCUser String
    -- | ServerJoin <user> <channel> - User joins channel.
    | ServerJoin BT.IRCUser BT.Channel
    -- | ServerPart <user> <channel> <message> - User leaves channel with the
    -- message.
    | ServerPart BT.IRCUser BT.Channel String
    -- | ServerTopic <user> <channel> <topic> - User set <topic> as topic for
    -- <channel>.
    | ServerTopic BT.IRCUser BT.Channel String
    -- | ServerInite <user> <nick> <channel> - User invited <nick> to <channel>.
    | ServerInvite BT.IRCUser BT.Nickname BT.Channel
    -- | ServerKick <user> <channel> <nick> - <nick> kicked from <channel> by
    -- <user>.
    | ServerKick BT.IRCUser BT.Channel BT.Nickname
    -- | ServerPrivMsg <user> <targets> <message> - Write <message> to <targets>
    -- from <user>.
    | ServerPrivMsg BT.IRCUser [Target] String
    -- | ServerNotice <server> <targets> <message> - Write message from <server>
    -- to <targets>.
    | ServerNotice BT.Servername [Target] String
    -- | ServerPing <server> - Ping coming from <server>.
    | ServerPing BT.Servername
    -- | ServerReply <server> <num> <args> <trailing> - Numeric reply <num> from
    -- <server> with arguments <args> and trailing argument <trailing>.
    | ServerReply BT.Servername Integer [String] (Maybe String)
  deriving (Show, Eq)

data Target = ChannelTarget BT.Channel
    | UserTarget BT.UserServer
    | NickTarget BT.IRCUser
  deriving (Show, Eq)

parseServerMessage :: T.Text -> Either P.ParseError ServerMessage
parseServerMessage = P.parse servermessage "(IRC message)"

servermessage :: P.Parsec T.Text () ServerMessage
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
        ]

nickMessage :: P.Parsec T.Text () ServerMessage
nickMessage =
    ServerNick <$> prefix nickUserHost <* P.string "NICK " <*> nickname

quitMessage :: P.Parsec T.Text () ServerMessage
quitMessage =
    ServerQuit <$> prefix nickUserHost <* P.string "QUIT " <*> trailing

joinMessage :: P.Parsec T.Text () ServerMessage
joinMessage =
    ServerJoin <$> prefix nickUserHost <* P.string "JOIN " <*> channel

partMessage :: P.Parsec T.Text () ServerMessage
partMessage = ServerPart <$> prefix nickUserHost <* P.string "PART " <*>
    channel <* P.char ' ' <*> trailing

topicMessage :: P.Parsec T.Text () ServerMessage
topicMessage = ServerTopic <$> prefix nickUserHost <* P.string "TOPIC " <*>
    channel <* P.char ' ' <*> trailing

inviteMessage :: P.Parsec T.Text () ServerMessage
inviteMessage = ServerInvite <$> prefix nickUserHost <* P.string "INVITE "
    <*> nickname <* P.char ' ' <*> channel

kickMessage :: P.Parsec T.Text () ServerMessage
kickMessage = ServerKick <$> prefix nickUserHost <* P.string "KICK " <*>
    channel <* P.char ' ' <*> nickname

privmsgMessage :: P.Parsec T.Text () ServerMessage
privmsgMessage = ServerPrivMsg <$> prefix nickUserHost <*
    P.string "PRIVMSG " <*> targets <* P.char ' ' <*> trailing

noticeMessage :: P.Parsec T.Text () ServerMessage
noticeMessage = ServerNotice <$> prefix servername <* P.string "NOTICE " <*>
    targets <* P.char ' ' <*> trailing

pingMessage :: P.Parsec T.Text () ServerMessage
pingMessage = ServerPing <$> (P.string "PING :" *> servername)

replyMessage :: P.Parsec T.Text () ServerMessage
replyMessage = ServerReply <$> prefix servername <*> P.decimal <* P.char ' ' <*>
    args <*> P.optionMaybe trailing

trailing :: P.Parsec T.Text () String
trailing = P.char ':' >> P.many (P.noneOf "\0\r\n")

nickname :: P.Parsec T.Text () BT.Nickname
nickname =
    consNick <$> P.alphaNum <*> P.many (P.choice [P.alphaNum, P.char '-', special])
  where
    consNick c1 str = case BT.nickname (c1:str) of
        Just nick -> nick
        Nothing -> error "The parsed nickname should be correct."

servername :: P.Parsec T.Text () BT.Servername
servername = hostname

channel :: P.Parsec T.Text () BT.Channel
channel = (:) <$> P.choice [P.char '#', P.char '+', P.char '&'] <*>
    P.many1 (P.noneOf "\0\a\r\n ,:")

targets :: P.Parsec T.Text () [Target]
targets = P.sepBy1 target (P.char ',')

target :: P.Parsec T.Text () Target
target = P.choice
    [ ChannelTarget <$> P.try channel
    , NickTarget <$> P.try forceNickUser_Host
    , UserTarget <$> P.try forceUserHost_Server
    , NickTarget <$> P.try nickUserHost
    , UserTarget <$> P.try userHostServer
    ]

nickUserHost :: P.Parsec T.Text () BT.IRCUser
nickUserHost = BT.IRCUser <$> nickname <*> user <*> host
  where
    user = P.optionMaybe $ P.char '!' *> username
    host = P.optionMaybe $ P.char '@' *> hostname

forceNickUser_Host :: P.Parsec T.Text () BT.IRCUser
forceNickUser_Host = BT.IRCUser <$> nickname <*> user <*> host
  where
    user = Just <$> (P.char '!' *> username)
    host = P.optionMaybe $ P.char '@' *> hostname

userHostServer :: P.Parsec T.Text () BT.UserServer
userHostServer = BT.UserServer <$> username <*> host <*> server
  where
    host = P.optionMaybe $ P.char '%' *> hostname
    server = P.optionMaybe $ P.char '@' *> servername

forceUserHost_Server :: P.Parsec T.Text () BT.UserServer
forceUserHost_Server = BT.UserServer <$> username <*> host <*> server
  where
    host = Just <$> (P.char '%' *> hostname)
    server = P.optionMaybe $ P.char '@' *> servername

special :: P.Parsec T.Text () Char
special = P.choice $ map P.char ['[', ']', '\\', '`', '_', '^', '{', '}', '|']

args :: P.Parsec T.Text () [String]
args = P.endBy (P.many $ P.noneOf "\0\r\n :") (P.char ' ')

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
hostname = hostname' <|> hostAddress

hostname' :: P.Parsec T.Text () BT.Hostname
hostname' = intercalate "." <$> P.sepBy1 shortname (P.char '.')

hostAddress :: P.Parsec T.Text () BT.Hostname
hostAddress = (ipV4ToString <$> P.try ipV4) <|>
    (ipV6ToString <$> P.try ipV6)

ipV4 :: P.Parsec T.Text () IPV4
ipV4 = IPV4 <$> (P.decimal <* period) <*> (P.decimal <* period)
    <*> (P.decimal <* period) <*> P.decimal
  where
    period = P.char '.'

ipV6 :: P.Parsec T.Text () IPV6
ipV6 = IPV6 <$> (P.hexnum <* colon) <*> (P.hexnum <* colon)
    <*> (P.hexnum <* colon) <*> (P.hexnum <* colon) <*> (P.hexnum <* colon)
    <*> (P.hexnum <* colon)  <*> (P.hexnum <* colon) <*> P.hexnum
  where
    colon = P.char ':'
