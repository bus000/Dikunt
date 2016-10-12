module Functions.Parrot
  ( parrot
  ) where

import qualified BotTypes as BT
import Data.List

parrot :: BT.BotFunction
parrot = BT.BotFunction
    { BT.shouldRun = shouldRun
    , BT.run = run
    , BT.help = "<nick>: <string> - Outputs string given."
    , BT.name = "Parrot"
    }

run :: BT.Message -> BT.Net [BT.Message]
run (BT.PrivMsg _ _ msg) = do
    nick <- BT.getValue BT.nickname

    BT.privmsgs $ parrot' nick msg
run _ = fail "Parrot should only run on PrivMsg's"

shouldRun :: BT.Message -> BT.Net Bool
shouldRun (BT.PrivMsg _ _ msg) = do
    nick <- BT.getValue BT.nickname
    return $ (nick ++ ": ") `isPrefixOf` msg
shouldRun _ = return False

parrot' :: String -> String -> String
parrot' nick = drop (length $ nick ++ ": ")
