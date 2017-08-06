{- |
 - Module      : Utils
 - Description : Utility functions for Dikunt.
 - Copyright   : (c) Magnus Stavngaard, 2017
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - Utility functions for Dikunt not belonging in any other module.
 -}
module Utils
    ( -- Help implementing shrink method for Arbitrary instance.
      shrink1
    , shrink2
    , shrink3
    ) where

import Test.QuickCheck (Arbitrary, shrink)

{- | Generate all shrinks for datatype with one argument. -}
shrink1 :: Arbitrary a => (a -> b)
    -- ^ Constructor.
    -> a
    -- ^ Initial value.
    -> [b]
shrink1 f a = [f a' | a' <- shrink a]

{- | Generate all shrinks for datatype with two arguments. -}
shrink2 :: Arbitrary a => Arbitrary b => (a -> b -> c)
    -- ^ Constructor.
    -> a
    -- ^ Initial value of first property.
    -> b
    -- ^ Initial value of second property.
    -> [c]
shrink2 f a b = [f a b' | b' <- shrink b] ++
    [f a' b | a' <- shrink a] ++
    [f a' b' | (a', b') <- shrink (a, b)]

{- | Generate all shrinks for datatype with three arguments. -}
shrink3 :: Arbitrary a => Arbitrary b => Arbitrary c => (a -> b -> c -> d)
    -- ^ Constructor.
    -> a
    -- ^ Initial value of first property.
    -> b
    -- ^ Initial value of second property.
    -> c
    -- ^ Initial value of third property.
    -> [d]
shrink3 f a b c = [f a b c' | c' <- shrink c] ++
    [f a b' c | b' <- shrink b] ++
    [f a b' c' | (b', c') <- shrink (b, c)] ++
    [f a' b c | a' <- shrink a] ++
    [f a' b c' | (a', c') <- shrink (a, c)] ++
    [f a' b' c | (a', b') <- shrink (a, b)] ++
    [f a' b' c' | (a', b', c') <- shrink (a, b, c)]
