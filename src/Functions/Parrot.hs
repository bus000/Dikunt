module Functions.Parrot
  ( parrot
  ) where

import qualified BotTypes as BT
import Data.List

parrot :: BT.BotFunction
parrot = BT.BotFunction
    { BT.shouldRun = parrotShouldRun
    , BT.run = runParrot
    , BT.help = "<nick>: <string> - Outputs string given."
    , BT.name = "Parrot"
    }

runParrot :: BT.Message -> BT.Net String
runParrot (BT.PrivMsg _ _ msg) = do
    nick <- BT.getValue BT.nickname
    return $ parrot' nick msg
runParrot _ = fail "Parrot should only run on PrivMsg's"

parrotShouldRun :: BT.Message -> BT.Net Bool
parrotShouldRun (BT.PrivMsg _ _ msg) = do
    nick <- BT.getValue BT.nickname
    return $ (nick ++ ": ") `isPrefixOf` msg
parrotShouldRun _ = return False

parrot' :: String -> String -> String
parrot' nick str = drop (length $ nick ++ ": ") str
