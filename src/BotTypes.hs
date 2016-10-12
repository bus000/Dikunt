module BotTypes
    ( Net
    , BotFunction(..)
    , Bot(..)
    , bot
    , getValue
    , message
    , Message(..)
    , privmsgs
    ) where

import Control.Applicative ((<|>))
import Control.Monad.State
import Safe (readMay)
import System.IO
import Text.Regex.PCRE ((=~))
import Data.Time.Clock (DiffTime)

{- | The state used by the bot. -}
type Net = StateT Bot IO

{- | The content of the state in the Net type. -}
data Bot = Bot
    { socket     :: Handle -- ^ The handle to communicate with the server.
    , nickname   :: String -- ^ The nickname used on the server.
    , channel    :: String -- ^ The channel connected to.
    , password   :: String -- ^ The password to connect with.
    , timeOffset :: DiffTime -- ^ Offset compared to UTC time.
    , functions  :: [BotFunction] -- ^ Functions handles messages to the bot.
    }

{- | A botfunction is a collection of functions that handles messages sent to
 - the bot. If shouldRun returns true for any given message then run is called
 - to generate the bots output. -}
data BotFunction = BotFunction
    { shouldRun :: Message -> Net Bool -- ^ If functions should handle message.
    , run       :: Message -> Net [Message] -- ^ Produce the bots output.
    , help      :: String -- ^ Generate a help string for the function.
    , name      :: String -- ^ The name of the function.
    }

{- | Represents an IRC command. -}
data IRCCommand = ADMIN | AWAY | CNOTICE | CPRIVMSG | CONNECT | DIE | ENCAP
    | ERROR | HELP | INFO | INVITE | ISON | JOIN | KICK | KILL | KNOCK | LINKS
    | LIST | LUSERS | MODE | MOTD | NAMES | NAMESX | NICK | NOTICE | OPER | PART
    | PASS | PING | PONG | PRIVMSG | QUIT | REHASH | RESTART | RULES | SERVER
    | SERVICE | SERVLIST | SQUERY | SQUIT | SETNAME | SILENCE | STATS | SUMMON
    | TIME | TOPIC | TRACE | UHNAMES | USER | USERHOST | USERIP | USERS
    | VERSION | WALLOPS | WATCH | WHO | WHOIS | WHOWAS | NUMCOM Integer
    deriving (Show, Read, Eq)

{- | Represent an IRC message of any type which is generally structured as
 - ":<prefix> <command> <params> :<trailing>\r\n". The prefix and trail might
 - not exist and is therefore of a Maybe type. -}
data IRCMessage = IRCMessage
    { _ircPrefix  :: Maybe String -- ^ The prefix of an IRC message.
    , _ircCommand :: IRCCommand -- ^ The command of the message.
    , _ircParams  :: [String] -- ^ List of parameters.
    , _trail      :: Maybe String -- ^ Actual message.
    } deriving (Show, Eq)

{- | Represents a collection of IRC messages that we have implemented support
 - for so far. -}
data Message = PrivMsg { privMsgFrom, privMsgTo, privMsgMessage :: String }
    | Ping { pingFrom :: String }
    | Join { joinNick :: String }
    | Quit { quitNick :: String }
    | Part { partNick :: String }
    | Pong { pongTo   :: String }
    | Nick { nicknick :: String }
    | User { userName :: String }
    deriving (Show, Eq, Read)

{- | Construct a Bot to handle a IRC chat. -}
bot :: Handle
    -- ^ Socket to an IRC server.
    -> String
    -- ^ Nickname to use on the server.
    -> String
    -- ^ Channel to connect to, should start with a #.
    -> String
    -- ^ Password to use.
    -> DiffTime
    -- ^ The time offset to use from UTC time.
    -> [BotFunction]
    -- ^ List of functions the server should support.
    -> Bot
bot h n c p o fs = Bot h n c p o fs

{- | Helper function to extract values from a Net state. -}
getValue :: (Bot -> a)
    -- ^ Extractor function.
    -> Net a
getValue f = get >>= \st -> return $ f st

{- | Construct an IRC message from a string send to a server. -}
ircMessage :: String
    -- ^ String to extract an IRCMessage from.
    -> Maybe IRCMessage
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

{- | Construct a Message from an IRCMessage. Messages only supports a subset of
 - IRCMessages. If the IRCMessage has a command not supported by message, the
 - function will return Nothing. -}
newMessage :: IRCMessage
    -- ^ IRCMessage to create a Message from.
    -> Maybe Message
newMessage (IRCMessage _ PING _ (Just trail)) = return $ Ping trail
newMessage (IRCMessage (Just prefix) PRIVMSG (to:_) (Just trail)) =
    let from = getNickname prefix in return $ PrivMsg from to trail
newMessage (IRCMessage (Just prefix) JOIN _ _) =
    let nick = getNickname prefix in return $ Join nick
newMessage (IRCMessage (Just prefix) QUIT _ _) =
    let nick = getNickname prefix in return $ Quit nick
newMessage (IRCMessage (Just prefix) PART _ _) =
    let nick = getNickname prefix in return $ Part nick
newMessage _ = Nothing

getNickname :: String -> String
getNickname = takeWhile ('!' /=) . drop 1

{- | Construct a Message from a string by using ircMessage. -}
message :: String
    -- ^ String to construct message from.
    -> Maybe Message
message str = ircMessage str >>= newMessage

privmsgs :: String -> Net [Message]
privmsgs msg = do
    nick <- getValue nickname
    chan <- getValue channel

    return $ map (PrivMsg nick chan) (lines msg)
