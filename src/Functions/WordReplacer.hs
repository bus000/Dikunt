module Functions.WordReplacer
  ( runReplaceWords
  ) where

import qualified BotTypes as BT

replacementList :: [(String, String)]
replacementList =
    [ ("Mark", "ShortGuy")
    , ("Jan", "Tjekkeren")
    , ("Magnus", "Glorious")
    , ("August", "Motherless")
    , ("Oleks", "Joleks")
    , ("10/10", "knæhøj karse")
    , ("ha!", "HAHAHAHAHAHHAHA!")
    , ("Mads", "Twin")
    , ("Troels", "Twin")
    , ("tjekker", "kontrollerer")
    , ("Tjekker", "Kontrollerer")
    , ("tjekke", "kontrollere")
    , ("vim", "best editor")
    , ("emacs", "worst editor")
    ]

replacementKeys :: [String]
replacementKeys = map fst replacementList

replaceWords :: String -> BT.Net (Maybe String)
replaceWords str
    | any (`elem` replacementKeys) (words str) =
        return . Just . unwords . map replace . words $ str
    | otherwise = return Nothing
  where
    replace word = case lookup word replacementList of
        Just replacement -> replacement
        _ -> word

runReplaceWords :: BT.BotFunction
runReplaceWords msg = replaceWords (BT.privMsgMessage msg)
