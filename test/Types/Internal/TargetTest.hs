module Types.Internal.TargetTest (tests) where

import Data.Aeson (encode, decode)
import Data.Maybe (isJust)
import Test.Tasty (TestTree , testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Types.Internal.Target (Target, Targets, getTargets, targets)

tests :: TestTree
tests = testGroup "Types.Internal.TargetTest"
    [ testProperty "jsonIdent" jsonIdent
    , testProperty "jsonIdent2" jsonIdent2
    , testProperty "minSize" minSize
    , testProperty "getIdent" getIdent
    , testProperty "combine" combine
    ]

jsonIdent :: Target -> Bool
jsonIdent targ = Just targ == (decode . encode) targ

jsonIdent2 :: Targets -> Bool
jsonIdent2 targs = Just targs == (decode . encode) targs

minSize :: Targets -> Bool
minSize = not . null . getTargets

getIdent :: Targets -> Bool
getIdent targs = Just targs == (targets . getTargets) targs

combine :: Target -> Target -> Bool
combine targ1 targ2 = isJust $ targets [targ1, targ2]
