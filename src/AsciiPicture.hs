module AsciiPicture
(
    picture,
) where

import System.Process

-- TODO: This should handle URLs aswell
picture :: String -> String -> IO String
picture img [] = picture img "--size=20x10"
picture (img) (sz) = do
                s <- readProcess "/usr/bin/jp2a" [img, sz] []
                return $ s
