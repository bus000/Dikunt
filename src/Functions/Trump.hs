module Functions.Trump
    ( runTrump
    ) where

import qualified BotTypes as BT
import Control.Monad.State
import Data.List
import Data.MarkovChain (run)
import System.IO (readFile)
import qualified System.Random as Random

runTrump :: BT.BotFunction
runTrump = do
    msg <- BT.getValue BT.message
    gen <- liftIO Random.newStdGen
    if "dikunt: trump" `isPrefixOf` msg
    then do
        res <- liftIO $ trump gen
        return $ Just res
    else return Nothing

trump :: Random.StdGen -> IO String
trump gen = do
    content <- readFile trumpFile
    let ws = words content
        trumpText = take 200 $ run 2 ws 0 gen

    return $ unwords trumpText

trumpFile :: FilePath
trumpFile = "./data/trump.txt"
