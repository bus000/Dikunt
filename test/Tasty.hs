module Main where

import qualified IRCMessageParser.Tests as IMPT
import Test.Tasty
    ( TestTree
    , testGroup
    , defaultMain
    )

allTests :: TestTree
allTests = testGroup "Tasty Tests" [ IMPT.tests ]

main :: IO ()
main = defaultMain allTests
