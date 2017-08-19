module Main where

import qualified Parsers.Tests
import qualified Types.Internal.ChannelTest
import qualified Types.Internal.HostnameTest
import qualified Tests
import Test.Tasty
    ( TestTree
    , testGroup
    , defaultMain
    )

allTests :: TestTree
allTests = testGroup "Tasty Tests"
    [ Tests.tests
    , Parsers.Tests.tests
    , Types.Internal.ChannelTest.tests
    , Types.Internal.HostnameTest.tests
    ]

main :: IO ()
main = defaultMain allTests
