module Main ( main ) where

import qualified BotTypes as BT
import Control.Monad (forever)
import Safe (readMay)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    forever $ do
        line <- getLine
        case readMay line :: Maybe BT.ServerMessage of
            Just mes -> handleMessage mes
            Nothing -> return ()

handleMessage :: BT.ServerMessage -> IO ()
handleMessage (BT.ServerJoin (BT.IRCUser nick _ _) _) =
    putStrLn $ "Welcome " ++ nick
handleMessage (BT.ServerQuit (BT.IRCUser nick _ _) _) =
    putStrLn $ "Goodbye " ++ nick
handleMessage (BT.ServerPart (BT.IRCUser nick _ _) _ _) =
    putStrLn $ "Goodbye " ++ nick
handleMessage _ = return ()
