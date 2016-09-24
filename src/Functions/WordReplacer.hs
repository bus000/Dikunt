module Functions.WordReplacer
  ( replaceWords
  ) where

replacementList :: [(String, String)]
replacementList =
  [ ("Mark", "ShortGuy")
  , ("Jan", "Tjekkeren")
  , ("Magnus", "Glorious")
  , ("August", "Motherless")
  , ("Oleks", "Joleks")
  , ("10/10", "knæhøj karse")
  , ("ha!", "HAHAHAHAHAHHAHA!")
  ]

replacementKeys :: [String]
replacementKeys = map fst replacementList

replacementVals :: [String]
replacementVals = map snd replacementList

replaceWords :: String -> IO (Maybe String)
replaceWords str
    | any ((flip elem) replacementKeys) (words str) =
        return . Just . unwords . map replace . words $ str
    | otherwise = return Nothing
  where
    replace word = case lookup word replacementList of
        Just replacement -> replacement
        _ -> word
