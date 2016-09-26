module Functions.Parrot
  ( runParrot
  ) where

import Data.List
import qualified BotTypes as BT

runParrot :: BT.BotFunction
runParrot = do
    str <- BT.getValue BT.message
    nick <- BT.getValue BT.nickname
    parrot nick str

parrot :: String -> String -> BT.Net (Maybe String)
parrot nick str
    | (nick ++ ": ") `isPrefixOf` str =
        return $ Just (drop (length $ nick ++ ": ") str)
    | otherwise = return Nothing
