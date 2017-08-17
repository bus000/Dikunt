{- |
 - Module      : Types.Internal.Nickname
 - Description : Nickname type which represents an IRC nickname.
 - Copyright   : (c) Magnus Stavngaard, 2017
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - Define types and instances for IRC nicknames which is a string that starts
 - with a letter and contains letters, numbers and any of the symbols
 - [-, [, ], \, `, ^, {, }].
 -}
{-# LANGUAGE OverloadedStrings #-}
module Types.Internal.Nickname where

import Text.Regex.PCRE ((=~))
import Data.Maybe (isJust)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)
import Test.QuickCheck.Gen (suchThat)
import qualified Data.Aeson.Types as Aeson
import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import qualified Data.Text as T

{- | IRC nickname that starts with a letter and after that is followed by string
 - of letters, numbers and any of the symbols [-, [, ], \, `, ^, {, }]. -}
newtype Nickname = Nickname String deriving (Show, Eq, Read)

{- | Smart constructor for Nicknames, only allow correct IRC nicknames to be
 - constructed. -}
nickname :: String
    -- ^ Nickname source.
    -> Maybe Nickname
nickname nick
    | nick =~ nicknameRegex = Just . Nickname $ nick
    | otherwise = Nothing
  where
    nicknameRegex = "^[A-z]([0-9A-z\\-\\[\\]\\\\\\`\\^\\{\\}])*$" :: String

{- | Get the actual nickname from the Nickname type. -}
getNickname :: Nickname
    -- ^ The Nickname to get nickname from.
    -> String
getNickname (Nickname nick) = nick

{- | Hardcoded nickname for the authentication server on freenode. -}
nickservNickname :: Nickname
nickservNickname = Nickname "NickServ"

{- | Construct arbitrary IRC nicknames. -}
instance Arbitrary Nickname where
    arbitrary = Nickname <$> suchThat arbitrary (isJust . nickname)

    shrink (Nickname nick) =
        [ Nickname nick' | nick' <- shrink nick
        , (isJust . nickname) nick'
        ]

{- | Convert Nickname's to JSON. -}
instance ToJSON Nickname where
    toJSON (Nickname nick) = Aeson.String . T.pack $ nick

{- | Parse Nickname's from JSON. -}
instance FromJSON Nickname where
    parseJSON = withText "nickname" $ return . Nickname . T.unpack
