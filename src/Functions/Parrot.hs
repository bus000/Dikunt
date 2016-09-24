module Functions.Parrot
  ( parrot
  ) where

import Data.List

parrot :: String -> IO (Maybe String)
parrot str
    | str `isPrefixOf` "dikunt: " = return $ Just (drop 8 $ str)
    | otherwise = return Nothing
