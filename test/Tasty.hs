module Main where

import qualified IRCMessageParser.Tests as IMPT
import qualified Tests as T
import Test.Tasty
    ( TestTree
    , testGroup
    , defaultMain
    )

allTests :: TestTree
allTests = testGroup "Tasty Tests" [ IMPT.tests , T.tests ]

main :: IO ()
main = defaultMain allTests
