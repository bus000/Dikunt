module Functions.Trump
    ( trump
    ) where

import qualified BotTypes as BT
import Control.Monad.State
import qualified Data.MarkovChain as MC
import System.IO (readFile)
import qualified System.Random as Random

trump :: BT.BotFunction
trump = BT.BotFunction
    { BT.shouldRun = shouldRun
    , BT.run = run
    , BT.help = "<nick>: trump - Outputs a markov random trump quote."
    , BT.name = "Trump"
    }

shouldRun :: BT.Message -> BT.Net Bool
shouldRun (BT.PrivMsg _ _ msg) = do
    nick <- BT.getValue BT.nickname
    case words msg of
        [first, second] ->
            return $ (first == nick ++ ":") && (second == "trump")
        _ -> return False
shouldRun _ = return False

run :: BT.Message -> BT.Net [BT.Message]
run BT.PrivMsg{} = do
    gen <- liftIO Random.newStdGen
    BT.privmsgs =<< (liftIO $ runTrump gen)
run _ = fail "Trump should only run on PrivMsg's."

runTrump :: Random.StdGen -> IO String
runTrump gen = do
    content <- readFile trumpFile
    let ws = words content
        trumpText = MC.run 2 ws 0 gen
        trumpText' = drop 1 $ dropWhile (notElem '.') trumpText
        sentence = takeWhile ('.' /=) (unwords trumpText') ++ "."

    return $ "Friends, delegates and fellow Americans: " ++ sentence

trumpFile :: FilePath
trumpFile = "./data/trump.txt"
