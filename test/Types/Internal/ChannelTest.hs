module Types.Internal.ChannelTest (tests) where

import Data.Aeson (encode, decode)
import Test.Tasty (TestTree , testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Types.Internal.Channel (Channel, getChannel, channel)

tests :: TestTree
tests = testGroup "Types.Internal.ChannelTest"
    [ testProperty "jsonIdent" jsonIdent
    , testProperty "startsWith" startsWith
    , testProperty "minSize" minSize
    , testProperty "getChannelIdent" getChannelIdent
    , testProperty "dontContain" dontContain
    ]

jsonIdent :: Channel -> Bool
jsonIdent chan = Just chan == (decode . encode) chan

startsWith :: Channel -> Bool
startsWith chan = (head . getChannel) chan `elem` "#+&"

minSize :: Channel -> Bool
minSize chan = (length . getChannel) chan >= 2

getChannelIdent :: Channel -> Bool
getChannelIdent chan = Just chan == (channel . getChannel) chan

dontContain :: Channel -> Bool
dontContain chan = not $ any (`elem` "\0\a\r\n ,:") (getChannel chan)
