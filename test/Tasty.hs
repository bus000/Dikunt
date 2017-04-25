module Main where

import qualified BotTypes.Tests as BTT
import qualified IRCMessageParser.Tests as IMPT
import Test.Tasty
    ( TestTree
    , testGroup
    , defaultMain
    )

allTests :: TestTree
allTests = testGroup "Tasty Tests" [ BTT.tests, IMPT.tests ]

main :: IO ()
main = defaultMain allTests
