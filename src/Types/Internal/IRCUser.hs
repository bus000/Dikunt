{- |
 - Module      : Types.Internal.IRCUser
 - Description : Representation of an IRC username.
 - Copyright   : (c) Magnus Stavngaard, 2017
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - An IRCUser is a nickname, together with maybe a username and hostname. The
 - IRCUser is mainly used to specify who sends messages to channels.
 -}
{-# LANGUAGE OverloadedStrings #-}
module Types.Internal.IRCUser where

import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)
import Types.Internal.Nickname (Nickname(..))
import Types.Internal.Hostname (Hostname(..))
import Types.Internal.Username (Username(..))
import Data.Aeson (ToJSON(..), FromJSON(..), withObject, (.=), (.:), (.:?), object)
import Utils (shrink3)

{- | Represents an IRC user which consist of a nickname, an optional username
 - and an optional hostname. In messages from IRC servers the message has a user
 - iff it starts with a ':' character. The user is then parsed as
 - "nickname [ [ "!" user ] "@" host ]". -}
data IRCUser = IRCUser Nickname (Maybe Username) (Maybe Hostname)
  deriving (Show, Read, Eq)

{- | Convert an IRCUser to JSON. -}
instance ToJSON IRCUser where
    toJSON (IRCUser nick Nothing Nothing) = object
        [ "nickname" .= nick
        ]

    toJSON (IRCUser nick Nothing (Just host)) = object
        [ "nickname" .= nick
        , "hostname" .= host
        ]

    toJSON (IRCUser nick (Just user) Nothing) = object
        [ "nickname" .= nick
        , "username" .= user
        ]

    toJSON (IRCUser nick (Just user) (Just host)) = object
        [ "nickname" .= nick
        , "username" .= user
        , "hostname" .= host
        ]

{- | Read an IRCUser from JSON. -}
instance FromJSON IRCUser where
    parseJSON = withObject "IRCUser" $ \o ->
        IRCUser <$> o .: "nickname" <*> o .:? "username" <*> o .:? "hostname"

{- | Construct arbitrary IRC users for testing. -}
instance Arbitrary IRCUser where
    arbitrary = IRCUser <$> arbitrary <*> arbitrary <*> arbitrary

    shrink (IRCUser nick user host) = shrink3 IRCUser nick user host
