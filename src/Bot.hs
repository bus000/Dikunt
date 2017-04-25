module Bot
    ( connect
    , disconnect
    , loop
    ) where

import qualified BotTypes as BT
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
    write h $ BT.ClientNick nick
    write h $ BT.ClientUser nick 0 "DikuntBot"
    write h $ BT.ClientPrivMsg (BT.IRCUser "NickServ" Nothing Nothing)
        ("IDENTIFY " ++ pass)
    write h $ BT.ClientJoin [(chan, "")]

    listen bot

listen :: BT.Bot -> IO ()
listen bot@(BT.Bot h _ _ _ _ _) = forever $ do
    s <- hGetLine h
    (ins, _) <- readMVar $ BT.pluginHandles bot

    case parseMessage (s ++ "\n") of
        Just (BT.ServerPing from) -> write h $ BT.ClientPong from
        Just message -> mapM_ (`hPrint` message) ins
        Nothing -> putStrLn $ "Could not parse message \"" ++ s ++ "\""

respond :: BT.Bot -> IO ()
respond bot@(BT.Bot h _ chan _ _ _) = forever $ do
    (_, output) <- readMVar $ BT.pluginHandles bot

    line <- hGetLine output
    case readMay line :: Maybe BT.ClientMessage of
        Just message -> write h message
        Nothing -> write h $ BT.ClientPrivMsgChan chan line

    threadDelay 1000000

write :: Handle -> BT.ClientMessage -> IO ()
write h mes = hPrintf h $ writeMessage mes
