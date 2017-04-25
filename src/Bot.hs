module Bot
    ( connect
    , disconnect
    , loop
    ) where

import qualified BotTypes as BT
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, readMVar, threadDelay)
import Control.Monad (forever)
import Data.Time.Clock (DiffTime)
import IRCParser.IRCMessageParser (parseMessage)
import IRCWriter.IRCWriter (writeMessage)
import Monitoring (startMonitoring)
import Network (connectTo, PortID(..))
import Safe (readMay)
import System.IO
    ( hClose
    , hSetBuffering
    , hGetLine
    , BufferMode(..)
    , hPrint
    , Handle
    )
import Text.Printf (hPrintf)

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

    handlesVar <- startMonitoring execs [nick, chan]

    let bot = BT.bot h nick chan pass diff handlesVar

    -- Start thread responding.
    _ <- forkIO $ respond bot

    return bot


loop :: BT.Bot -> IO ()
loop bot@(BT.Bot h nick chan pass _ _) = do
    writeNew h $ BT.ClientNick nick
    writeNew h $ BT.ClientUser nick 0 "DikuntBot"
    writeNew h $ BT.ClientPrivMsg (BT.IRCUser "NickServ" Nothing Nothing)
        ("IDENTIFY " ++ pass)
    writeNew h $ BT.ClientJoin [(chan, "")]

    listen bot

listen :: BT.Bot -> IO ()
listen bot = forever $ do
    s <- hGetLine h
    (ins, _) <- readMVar $ BT.pluginHandles bot

    case parseMessage (s ++ "\n") of
        Just (BT.ServerPing from) -> writeNew h $ BT.ClientPong from
        Just message -> mapM_ (`hPrint` message) ins
        Nothing -> putStrLn $ "Could not parse message \"" ++ s ++ "\""
  where
    h = BT.socket bot

respond :: BT.Bot -> IO ()
respond bot@(BT.Bot h _ chan _ _ _) = forever $ do
    (_, output) <- readMVar $ BT.pluginHandles bot

    line <- hGetLine output
    case readMay line :: Maybe BT.ClientMessage of
        Just message -> writeNew h message
        Nothing -> writeNew h $ BT.ClientPrivMsgChan chan line

    threadDelay 1000000

writeNew :: Handle -> BT.ClientMessage -> IO ()
writeNew h mes = hPrintf h $ writeMessage mes

{-write :: Handle -> BT.Message -> IO ()-}
{-write h (BT.PrivMsg _ to msg) =-}
    {-hPrintf h "PRIVMSG %s :%s\r\n" to msg-}
{-write h (BT.Pong to) =-}
    {-hPrintf h "PONG %s\r\n" to-}
{-write h (BT.Nick name) =-}
    {-hPrintf h "NICK %s\r\n" name-}
{-write h (BT.User name) =-}
    {-hPrintf h "USER %s\r\n" name-}
{-write h (BT.Join chan) =-}
    {-hPrintf h "JOIN %s\r\n" chan-}
{-write _ _ =-}
    {-putStrLn "Cannot send message type"-}
