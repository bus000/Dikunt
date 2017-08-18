module Types.Internal.ChannelTest (tests) where

import Data.Aeson (encode, decode)
import Test.Tasty (TestTree , testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Types.Internal.Channel (Channel, getChannel)

tests :: TestTree
tests = testGroup "Channel Tests"
    [ testProperty "json identity" jsonIdent
    , testProperty "starts with correct" startsWith
    ]

jsonIdent :: Channel -> Bool
jsonIdent chan = Just chan == (decode . encode) chan

startsWith :: Channel -> Bool
startsWith chan = (head . getChannel) chan `elem` "#+&"
