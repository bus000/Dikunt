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
    , lastMessage
    , saveMsg
    ) where

import Control.Monad.State
import System.IO
import Text.Regex.Posix ((=~))
import Data.Char (toUpper, isSpace)
import Safe (readMay)

type Net = StateT Bot IO
data Bot = Bot
    { socket         :: Handle
    , nickname       :: String
    , channel        :: String
    , password       :: String
    , currentMessage :: Maybe Message
    , lastMessage    :: Maybe Message
    }

{- | Defines a Dikunt action. All new Dikunt features should implement a
 - function of this type and report in the functions list. -}
type BotFunction = Message -> Net (Maybe String)

data IRCCommand = ADMIN | AWAY | CNOTICE | CPRIVMSG | CONNECT | DIE | ENCAP
    | ERROR | HELP | INFO | INVITE | ISON | JOIN | KICK | KILL | KNOCK | LINKS
    | LIST | LUSERS | MODE | MOTD | NAMES | NAMESX | NICK | NOTICE | OPER | PART
    | PASS | PING | PONG | PRIVMSG | QUIT | REHASH | RESTART | RULES | SERVER
    | SERVICE | SERVLIST | SQUERY | SQUIT | SETNAME | SILENCE | STATS | SUMMON
    | TIME | TOPIC | TRACE | UHNAMES | USER | USERHOST | USERIP | USERS
    | VERSION | WALLOPS | WATCH | WHO | WHOIS | WHOWAS
    deriving (Show, Read, Eq)

data Message = Message
    { senderNick    :: String
    , senderIP      :: String
    , command       :: IRCCommand
    , messageString :: String
    } deriving (Show)

bot :: Handle -> String -> String -> String -> Bot
bot h n c p = Bot h n c p Nothing Nothing

saveMsg :: String -> Net ()
saveMsg msg = do
    st <- get
    put st { lastMessage = currentMessage st
        , currentMessage = message msg
        }

getValue :: (Bot -> a) -> Net a
getValue f = get >>= \st -> return $ f st

message :: String -> Maybe Message
message str = case matchGroups of
    [[_, user, ip, com, chan, msg]] -> do
        com' <- ircMode com
        return $ Message user ip com' msg
    _ -> Nothing
  where
    matchGroups = str =~ ":(.*)!~(.*) (.*) (#.*) :(.*)" :: [[String]]

ircMode :: String -> Maybe IRCCommand
ircMode = readMay . map toUpper . filter (not . isSpace)


    {-["ADMIN"] -> ADMIN-}
    {-["AWAY"] -> AWAY-}
    {-["CNOTICE"] -> CNOTICE-}
    {-["CPRIVMSG"] -> CPRIVMSG-}
    {-["CONNECT"] -> CONNECT-}
    {-["DIE"] -> DIE-}
    {-["ENCAP"] -> ENCAP-}
    {-["ERROR"] -> ERROR-}
    {-["HELP"] -> HELP-}
    {-["INFO"] -> INFO-}
    {-["INVITE"] -> INVITE-}
    {-["ISON"] -> ISON-}
    {-["JOIN"] -> JOIN-}
    {-["KICK"] -> KICK-}
    {-["KILL"] -> KILL-}
    {-["KNOCK"] -> KNOCK-}
    {-["LINKS"] -> LINKS-}
    {-["LIST"] -> LIST-}
    {-["LUSERS"] -> LUSERS-}
    {-["MODE"] -> MODE-}
    {-["MOTD"] -> MOTD-}
    {-["NAMES"] -> NAMES-}
    {-["NAMESX"] -> NAMESX-}
    {-["NICK"] -> NICK-}
    {-["NOTICE"] -> NOTICE-}
    {-["OPER"] -> OPER-}
    {-["PART"] -> PART-}
    {-["PASS"] -> PASS-}
    {-["PING"] -> PING-}
    {-["PONG"] -> PONG-}
    {-["PRIVMSG"] -> PRIVMSG-}
    {-["QUIT"] -> QUIT-}
    {-["REHASH"] -> REHASH-}
    {-["RESTART"] -> RESTART-}
    {-["RULES"] -> RULES-}
    {-["SERVER"] -> SERVER-}
    {-["SERVICE"] -> SERVICE-}
    {-["SERVLIST"] -> SERVLIST-}
    {-["SQUERY"] -> SQUERY-}
    {-["SQUIT"] -> SQUIT-}
    {-["SETNAME"] -> SETNAME-}
    {-["SILENCE"] -> SILENCE-}
    {-["STATS"] -> STATS-}
    {-["SUMMON"] -> SUMMON-}
    {-["TIME"] -> TIME-}
    {-["TOPIC"] -> TOPIC-}
    {-["TRACE"] -> TRACE-}
    {-["UHNAMES"] -> UHNAMES-}
    {-["USER"] -> USER-}
    {-["USERHOST"] -> USERHOST-}
    {-["USERIP"] -> USERIP-}
    {-["USERS"] -> USERS-}
    {-["VERSION"] -> VERSION-}
    {-["WALLOPS"] -> WALLOPS-}
    {-["WATCH"] -> WATCH-}
    {-["WHO"] -> WHO-}
    {-["WHOIS"] -> WHOIS-}
    {-["WHOWAS"] -> WHOWAS-}
    {-_ -> Undefined-}

{-":bus000_!~fluttersh@46.101.150.96 PRIVMSG #dikufags :asciiart: (y)" =~ ":(.*)!~(.*) (.*) (#.*) :(.*)" :: [[String]]-}
