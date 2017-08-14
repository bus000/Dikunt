{- |
 - Module      : Bot
 - Description : Start, maintain and end connection to IRC channel.
 - Copyright   : (c) Magnus Stavngaard, 2016
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - Module exports 3 functions that are used to create, maintain and stop bots
 - running on IRC channels. The function connect takes configuration data and
 - sets up a connection to an IRC server. The function loop reads and parses
 - messages from the IRC server and pass them on to plugins handling the
 - messages. The function disconnect drops the connection to the IRC server.
 -}
{-# LANGUAGE OverloadedStrings #-}
module Bot
    ( connect
    , runBot
    , disconnect
    ) where

import qualified Types.BotTypes as BT
import Control.Concurrent (threadDelay, forkFinally)
import Control.Concurrent.MVar (newEmptyMVar, tryPutMVar, readMVar)
import Control.Exception (Exception, throw)
import Control.Monad (forever, mapM_, void)
import Data.Aeson (encode, decode, FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Data.Typeable (Typeable)
import Parsers.IRCMessageParser (parseMessage)
import IRCWriter.IRCWriter (writeMessage)
import Monitoring (startMonitoring, writeAll, readContent, stopMonitoring)
import Network (connectTo, PortID(..))
import System.IO (hClose, hSetBuffering, BufferMode(..), Handle, hPutStr, stdin, hFlush, stdout)
import qualified System.Log.Logger as Log

{- | Custom dikunt bot errors. -}
data BotThreadStopped
    -- | Exception thrown when any bot thread has stopped.
    = BotThreadStopped
  deriving (Show, Typeable)

{- | Make bot exception an actual Haskell exception. -}
instance Exception BotThreadStopped

{- | Connect the bot to an IRC server with the channel, nick, pass and port
 - given. Starts a monitor for the list of plugins given which will maintain a
 - running copy of each plugin. -}
connect :: BT.BotConfig
    -- ^ Configuration for bot.
    -> [FilePath]
    -- ^ Plugins to load.
    -> [String]
    -- ^ Extra plugin args.
    -> IO BT.Bot
connect (BT.BotConfig serv nick pass chan port) execs args = do
    h <- connectTo (BT.getServerName serv) (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering

    -- Start all plugins and a monitor for them.
    monitor <- startMonitoring execs
        (BT.getNickname nick:BT.getChannel chan:args)

    -- Create MVar used to stop the bot when any bot thread terminates.
    closedMVar <- newEmptyMVar

    -- Create the bot.
    let bot = BT.bot h nick chan pass monitor closedMVar

    -- Start thread reading from channel and propagating to plugins.
    void $ forkFinally (listen bot) (reportStopped closedMVar)

    -- Start thread reading from plugins and propagating messages to channel.
    void $ forkFinally (respond bot) (reportStopped closedMVar)

    -- Start thread reading from stdin and propagating messages to channel.
    void $ forkFinally (respondAdmin bot) (reportStopped closedMVar)

    return bot
  where
    reportStopped mvar _ = void $ tryPutMVar mvar ()

{- | Keeps track of the running of the bot. Raises an exception if any thread
 - connected to the bot dies. -}
runBot :: BT.Bot
    -- ^ Bot to listen for messages from.
    -> IO ()
runBot (BT.Bot _ _ _ _ _ closedMVar) = do
    void $ readMVar closedMVar
    throw BotThreadStopped

{- | Disconnect a bot from the server it is connected to. The functions should
 - be called when a bot is no longer used. -}
disconnect :: BT.Bot
    -- ^ Bot to disconnect.
    -> IO ()
disconnect (BT.Bot h _ _ _ monitor _) = do
    write h $ BT.ClientQuit "Higher powers"
    stopMonitoring monitor
    hClose h

{- | Register on channel with nickname and listen for messages from the server
 - the bot is connected to. Messages are passed to the list of plugins in the
 - bot. -}
listen :: BT.Bot
    -- ^ Bot to listen for messages for.
    -> IO ()
listen bot@(BT.Bot h nick chan pass _ _) = do
    write h $ BT.ClientNick nick
    write h $ BT.ClientUser user 0 "DikuntBot"
    write h $ BT.ClientPrivMsg (BT.IRCUser BT.nickservNickname Nothing Nothing)
        ("IDENTIFY " ++ BT.getNickname nick ++ " " ++ pass)
    write h $ BT.ClientJoin [(chan, "")]

    s <- B.hGetContents h
    mapM_ (handleMessage bot) $ messages s
  where
    messages = map (`T.append` "\n") . T.lines . T.decodeUtf8
    Just user = BT.username $ BT.getNickname nick

{- | Handle a message from the IRC channel. If the message is a ping a pong
 - response is sent otherwise the message is sent to all plugins. The message is
 - also logged. -}
handleMessage :: BT.Bot
    -- ^ Bot that received the message.
    -> T.Text
    -- ^ The message.
    -> IO ()
handleMessage (BT.Bot h _ _ _ monitor _) str = case parseMessage str of
    Right (BT.ServerPing from) -> hPutStr h $ writeMessage (BT.ClientPong from)
    Right message -> do
        iLog (BT.getServerCommand message) (show message)
        writeAll monitor $ jsonEncode message
    Left err -> eLog $ "Could not parse message \"" ++
        (init . init) (T.unpack str) ++ "\" with error message \"" ++
        show err ++ "\""
  where
    eLog = Log.errorM "bot.handleMessage"
    iLog c = Log.infoM $ "messages." ++ show c ++ ".received"

{- | Read from the output part of the pipe in the monitor and write messages
 - produced to the IRC channel. The function sleeps for 1 second after each
 - message write. If one sends messages to fast to IRC you are disconnected. -}
respond :: BT.Bot
    -- ^ Bot to respond for.
    -> IO ()
respond (BT.Bot h _ chan _ monitor _) = do
    messages <- map decodeOrPriv . T.lines <$> readContent monitor
    mapM_ (\mes -> write h mes >> threadDelay 1000000) messages
  where
    decodeOrPriv str = fromMaybe (BT.ClientPrivMsgChan chan (T.unpack str)) $
        jsonDecode str

{- | Read from stdin and replicate the strings to the IRC channel. Can be used
 - by an administrator to write messages for Dikunt. -}
respondAdmin :: BT.Bot -> IO ()
respondAdmin (BT.Bot h _ chan _ _ _) = forever $ do
    putStr " > " >> hFlush stdout
    line <- T.hGetLine stdin
    case jsonDecode line of
        Just message -> write h message
        Nothing -> write h $ BT.ClientPrivMsgChan chan (T.unpack line)

{- | Write a clientmessage to a server handle. -}
write :: Handle
    -- ^ Server handle to write to.
    -> BT.ClientMessage
    -- ^ Message to write.
    -> IO ()
write h msg = do
    Log.infoM ("messages." ++ show command ++ ".send") $ show msg
    hPutStr h $ writeMessage msg
  where
    command = BT.getClientCommand msg

{- | Encode a value as JSON in a Lazy UTF8 text format. -}
jsonEncode :: ToJSON a => a
    -- ^ Value to encode.
    -> T.Text
jsonEncode = T.decodeUtf8 . encode

{- | Decode a value from a Lazy UTF8 text JSON. -}
jsonDecode :: FromJSON a => T.Text
    -- ^ Text to decode.
    -> Maybe a
jsonDecode = decode . T.encodeUtf8
