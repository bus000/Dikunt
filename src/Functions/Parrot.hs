module Functions.Parrot
  ( parrot
  ) where

import Data.List

parrot :: String -> IO (Maybe String)
parrot str
    | "dikunt: " `isPrefixOf` str = return $ Just (drop 8 str)
    | otherwise = return Nothing
