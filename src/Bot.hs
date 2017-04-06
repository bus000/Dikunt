module Bot
    ( connect
    , disconnect
    , loop
    ) where

import Monitoring (startMonitoring)
import Control.Concurrent (forkIO, readMVar, threadDelay)
import System.IO (Handle)
import Control.Monad (forever)
import Data.Time.Clock (DiffTime)
import Network (connectTo, PortID(..))
import System.IO (hClose, hSetBuffering, hGetLine, BufferMode(..), hPutStrLn)
import Text.Printf (hPrintf)

-- Bot modules
import qualified BotTypes as BT

disconnect :: BT.Bot -> IO ()
disconnect = hClose . BT.socket

{- | Connect the bot to an IRC server with the channel, nick, pass and port
 - given. -}
connect :: String
    -- ^ URL to server to connect to.
    -> String
    -- ^ Channel name to join.
    -> String
    -- ^ Nickname to use.
    -> String
    -- ^ Password to connect with.
    -> Integer
    -- ^ Port to use.
    -> DiffTime
    -- ^ Difference in time of clients compared to UTC.
    -> [FilePath]
    -- ^ Plugins to load.
    -> IO BT.Bot
connect serv chan nick pass port diff execs = do
    h <- connectTo serv (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering

    handlesVar <- startMonitoring execs

    let bot = BT.bot h nick chan pass diff handlesVar

    -- Start thread responding.
    _ <- forkIO $ respond bot

    return bot


loop :: BT.Bot -> IO ()
loop bot = do
    write h $ BT.Nick nick
    write h $ BT.User (nick ++ " 0 * :DikuntBot")
    write h $ BT.PrivMsg nick "NickServ" ("IDENTIFY" ++ nick ++ pass)
    write h $ BT.Join chan
    listen bot
  where
    h = BT.socket bot
    pass = BT.password bot
    nick = BT.nickname bot
    chan = BT.channel bot

listen :: BT.Bot -> IO ()
listen bot = forever $ do
    s <- fmap init $ hGetLine h
    (ins, _) <- readMVar $ BT.pluginHandles bot

    case BT.message s of
        Just (BT.Ping from) -> write h $ BT.Pong from
        Just message -> mapM_ (\handle -> hPutStrLn handle $ show message) ins
        Nothing -> putStrLn $ "Could not parse message" ++ s

    threadDelay 1000000
  where
    h = BT.socket bot
    chan = BT.channel bot

respond :: BT.Bot -> IO ()
respond bot = forever $ do
    (_, output) <- readMVar $ BT.pluginHandles bot

    line <- hGetLine output
    write h $ BT.PrivMsg nick chan line
  where
    h = BT.socket bot
    nick = BT.nickname bot
    chan = BT.channel bot

write :: Handle -> BT.Message -> IO ()
write h (BT.PrivMsg _ to msg) =
    hPrintf h "PRIVMSG %s :%s\r\n" to msg
write h (BT.Pong to) =
    hPrintf h "PONG %s\r\n" to
write h (BT.Nick name) =
    hPrintf h "NICK %s\r\n" name
write h (BT.User name) =
    hPrintf h "USER %s\r\n" name
write h (BT.Join chan) =
    hPrintf h "JOIN %s\r\n" chan
write _ _ =
    putStrLn "Cannot send message type"
