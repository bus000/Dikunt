module BotTypes
    ( Net
    , BotFunction(..)
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
import Safe (readMay)
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

data BotFunction = BotFunction
    { shouldRun :: Message -> Net Bool
    , run       :: Message -> Net String
    , help      :: String
    , name      :: String
    }

data IRCCommand = ADMIN | AWAY | CNOTICE | CPRIVMSG | CONNECT | DIE | ENCAP
    | ERROR | HELP | INFO | INVITE | ISON | JOIN | KICK | KILL | KNOCK | LINKS
    | LIST | LUSERS | MODE | MOTD | NAMES | NAMESX | NICK | NOTICE | OPER | PART
    | PASS | PING | PONG | PRIVMSG | QUIT | REHASH | RESTART | RULES | SERVER
    | SERVICE | SERVLIST | SQUERY | SQUIT | SETNAME | SILENCE | STATS | SUMMON
    | TIME | TOPIC | TRACE | UHNAMES | USER | USERHOST | USERIP | USERS
    | VERSION | WALLOPS | WATCH | WHO | WHOIS | WHOWAS | NUMCOM Integer
    deriving (Show, Read, Eq)

data IRCMessage = IRCMessage
    { _ircPrefix  :: Maybe String
    , _ircCommand :: IRCCommand
    , _ircParams  :: [String]
    , _trail      :: Maybe String
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
    | Join { joinNick :: String }
    | Quit { quitNick :: String }
    deriving (Show, Eq, Read)

newMessage :: IRCMessage -> Maybe Message
newMessage (IRCMessage _ PING _ (Just trail)) = return $ Ping trail
newMessage (IRCMessage (Just prefix) PRIVMSG (to:_) (Just trail)) =
    let from = takeWhile ('!' /=) prefix in return $ PrivMsg from to trail
newMessage (IRCMessage (Just prefix) JOIN _ _) =
    let nick = takeWhile ('!' /=) . drop 1 $ prefix in return $ Join nick
newMessage (IRCMessage (Just prefix) QUIT _ _) =
    let nick = takeWhile ('!' /=) . drop 1 $ prefix in return $ Quit nick
newMessage _ = Nothing

message :: String -> Maybe Message
message str = ircMessage str >>= newMessage

{-IRCMessage {ircPrefix = Just ":bus000_!~fluttersh@46.101.150.96 ", ircCommand = QUIT, ircParams= [], trail = Just "Quit: leaving"}-}

{-IRCMessage {ircPrefix = Just ":bus000!~fluttersh@46.101.150.96 ", ircCommand = JOIN, ircParams = ["#dikufags_test"], trail = Nothing}-}

