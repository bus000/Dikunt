module Functions.WordReplacer
  ( wordReplacer
  ) where

import qualified BotTypes as BT

wordReplacer :: BT.BotFunction
wordReplacer = BT.BotFunction
    { BT.shouldRun = wordReplacerShouldRun
    , BT.run = runReplaceWords
    , BT.help = "Replaces words in any message."
    , BT.name = "WordReplacer"
    }

wordReplacerShouldRun :: BT.Message -> BT.Net Bool
wordReplacerShouldRun (BT.PrivMsg _ _ msg) =
    return $ any (`elem` replacementKeys) (words msg)
wordReplacerShouldRun _ = return False

runReplaceWords :: BT.Message -> BT.Net [BT.Message]
runReplaceWords (BT.PrivMsg _ _ msg) =
    BT.privmsgs . unwords . map replace . words $ msg
  where
    replace word = case lookup word replacementList of
        Just replacement -> replacement
        _ -> word
runReplaceWords _ = fail "WordReplacer should only run on PrivMsg's."

replacementKeys :: [String]
replacementKeys = map fst replacementList

replacementList :: [(String, String)]
replacementList =
    [ ("Mark", "ShortGuy")
    , ("Jan", "Tjekkeren")
    , ("Magnus", "Glorious")
    , ("August", "Motherless")
    , ("Oleks", "Joleks")
    , ("10/10", "knæhøj karse")
    , ("ha!", "HAHAHAHAHAHHAHA!")
    , ("haha", "hilarious, just hilarious")
    , ("Mads", "Twin")
    , ("Troels", "Twin")
    , ("tjekker", "kontrollerer")
    , ("Tjekker", "Kontrollerer")
    , ("tjekke", "kontrollere")
    , ("vim", "best editor")
    , ("emacs", "worst editor")
    , ("fucking", "")
    , ("fuck", "")
    , ("Fucking", "")
    , ("Fuck", "")
    , ("Helvede", "Himlen")
    , ("helvede", "himlen")
    , ("0/10", "Mark's Bot")
    , ("danmark", "I DANMARK ER JEG FØDT DER HAR JEG HJEEEEEME")
    , ("Danmark", "I DANMARK ER JEG FØDT DER HAR JEG HJEEEEEME")
    , ("USA", "Murica'")
    , ("Margrethe", "Hendes Majestæt Dronning Margrethe II")
    , ("Henrik", "Hans Kongelige Højhed Prins Henrik")
    , ("Frederik", "Hans Kongelige Højhed Kronprins Frederik, Prins til Danmark, greve af Monpezat")
    , ("Joakim", "Hans Kongelige Højhed Prins Joachim")
    , ("Mary", "Hendes Kongelige Højhed Kronprinsesse Mary")
    , ("python", "python2.7")
    , ("Python", "Python2.7")
    , ("latex", "LaTeX")
    , ("tex", "Tex")
    ]
