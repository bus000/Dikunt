module Functions.AsciiPicture
  ( runAsciiPicture,
  ) where

import System.Process

runAsciiPicture :: String -> IO (Maybe String)
runAsciiPicture = undefined

-- TODO: This should handle URLs aswell
picture :: String -> String -> IO String
picture img [] = picture img "--size=20x10"
picture (img) (sz) = do
    s <- readProcess "/usr/bin/jp2a" [img, sz] []
    return $ s
