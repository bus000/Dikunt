module BotTypes
    ( Net
    , BotFunction
    , Bot
    , bot
    , getValue
    , socket
    , nickname
    , channel
    , password
    , message
    , Message(..)
    ) where

import Control.Applicative ((<|>))
import Control.Monad.State
import Safe (readMay, atMay)
import System.IO
import Text.Regex.PCRE ((=~))

type Net = StateT Bot IO
data Bot = Bot
    { socket   :: Handle
    , nickname :: String
    , channel  :: String
    , password :: String
    }

bot :: Handle -> String -> String -> String -> Bot
bot h n c p = Bot h n c p

getValue :: (Bot -> a) -> Net a
getValue f = get >>= \st -> return $ f st

{- | Defines a Dikunt action. All new Dikunt features should implement a
 - function of this type and report in the functions list. -}
type BotFunction = Message -> Net (Maybe String)

data IRCCommand = ADMIN | AWAY | CNOTICE | CPRIVMSG | CONNECT | DIE | ENCAP
    | ERROR | HELP | INFO | INVITE | ISON | JOIN | KICK | KILL | KNOCK | LINKS
    | LIST | LUSERS | MODE | MOTD | NAMES | NAMESX | NICK | NOTICE | OPER | PART
    | PASS | PING | PONG | PRIVMSG | QUIT | REHASH | RESTART | RULES | SERVER
    | SERVICE | SERVLIST | SQUERY | SQUIT | SETNAME | SILENCE | STATS | SUMMON
    | TIME | TOPIC | TRACE | UHNAMES | USER | USERHOST | USERIP | USERS
    | VERSION | WALLOPS | WATCH | WHO | WHOIS | WHOWAS | NUMCOM Integer
    deriving (Show, Read, Eq)

data IRCMessage = IRCMessage
    { ircPrefix  :: Maybe String
    , ircCommand :: IRCCommand
    , ircParams  :: [String]
    , trail      :: Maybe String
    } deriving (Show, Eq)

ircMessage :: String -> Maybe IRCMessage
ircMessage str = do
    cmd' <- numericCommand <|> textCommand
    return $ IRCMessage prefix' cmd' args' msg'
  where
    pattern = "^(:(\\S+) )?(\\S+)( (?!:)(.+?))?( :(.+))?$"
    [[_, prefix, _, cmd, _, args, _, msg]] = str =~ pattern :: [[String]]
    textCommand = readMay cmd
    numericCommand = readMay cmd >>= return . NUMCOM

    prefix' = if null prefix then Nothing else Just prefix
    args' = words args
    msg' = if null msg then Nothing else Just msg

data Message = PrivMsg { privMsgFrom, privMsgTo, privMsgMessage :: String }
    | Ping { pingFrom :: String }
    deriving (Show, Eq, Read)

newMessage :: IRCMessage -> Maybe Message
newMessage msg
    | ircCommand msg == PING = do
        from <- trail msg
        return $ Ping from
    | ircCommand msg == PRIVMSG = do
        prefix <- ircPrefix msg
        let from = takeWhile ('!' /=) prefix
        to <- atMay (ircParams msg) 0
        str <- trail msg
        return $ PrivMsg from to str
    | otherwise = Nothing

message :: String -> Maybe Message
message str = ircMessage str >>= newMessage
