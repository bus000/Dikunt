module Functions.Parrot
  ( runParrot
  ) where

import Data.List
import qualified BotTypes as BT

runParrot :: BT.BotFunction
runParrot msg = do
    nick <- BT.getValue BT.nickname
    parrot nick (BT.messageString msg)

parrot :: String -> String -> BT.Net (Maybe String)
parrot nick str
    | (nick ++ ": ") `isPrefixOf` str =
        return $ Just (drop (length $ nick ++ ": ") str)
    | otherwise = return Nothing
