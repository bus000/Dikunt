module Types.Internal.NicknameTest (tests) where

import Data.Aeson (encode, decode)
import qualified Data.Char as Char
import Test.Tasty (TestTree , testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Types.Internal.Nickname (Nickname, getNickname, nickname)

tests :: TestTree
tests = testGroup "Types.Internal.NicknameTest"
    [ testProperty "jsonIdent" jsonIdent
    , testProperty "startsWith" startsWith
    , testProperty "minSize" minSize
    , testProperty "getIdent" getIdent
    , testProperty "contains" contains
    ]

jsonIdent :: Nickname -> Bool
jsonIdent nick = Just nick == (decode . encode) nick

minSize :: Nickname -> Bool
minSize = not . null . getNickname

getIdent :: Nickname -> Bool
getIdent nick = Just nick == (nickname . getNickname) nick

contains :: Nickname -> Bool
contains = all (\x -> Char.isAlphaNum x || x `elem` "-[]\\`_^{}|") . getNickname

startsWith :: Nickname -> Bool
startsWith = Char.isAlphaNum . head . getNickname
