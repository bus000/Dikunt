module Main ( main ) where

import qualified BotTypes as BT
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Safe (readMay)
import Control.Monad (forever)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    forever $ do
        line <- getLine
        case readMay line :: Maybe BT.Message of
            Just mes -> handleMessage mes
            Nothing -> return ()

handleMessage :: BT.Message -> IO ()
handleMessage (BT.Join nick) = putStrLn $ "Welcome " ++ nick
handleMessage (BT.Quit nick) = putStrLn $ "Goodbye " ++ nick
handleMessage (BT.Part nick) = putStrLn $ "Goodbye " ++ nick
handleMessage _ = return ()
