{- |
 - Module      : Types.Internal.Target
 - Description : Representation of an IRC username.
 - Copyright   : (c) Magnus Stavngaard, 2017
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - A target is the receiver of any IRC operation. Most commonly it is the
 - receiver of either PRIVMSG's or NOTICE's.
 -}
{-# LANGUAGE OverloadedStrings #-}
module Types.Internal.Target where

import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)
import Test.QuickCheck.Gen (oneof, listOf1)
import Types.Internal.Channel (Channel)
import Types.Internal.UserServer (UserServer)
import Types.Internal.IRCUser (IRCUser)
import Data.Aeson (ToJSON(..), FromJSON(..), withObject, (.=), (.:?), object)
import Utils (shrink1)

{- | Define an IRC target. -}
data Target
    -- | Make an IRC channel a target.
    = ChannelTarget Channel
    -- | Make an IRC user a target.
    | UserTarget UserServer
    -- | Make an IRC user a target.
    | NickTarget IRCUser
  deriving (Show, Read, Eq)

{- | Convert a Target to JSON. -}
instance ToJSON Target where
    toJSON (ChannelTarget chan) = object
        [ "channel" .= chan
        ]
    toJSON (UserTarget user) = object
        [ "username" .= user
        ]
    toJSON (NickTarget user) = object
        [ "nickname" .= user
        ]

{- | Parse a target from JSON. -}
instance FromJSON Target where
    parseJSON = withObject "Target" $ \o -> do
        chan <- o .:? "channel"
        user <- o .:? "username"
        nick <- o .:? "nickname"
        case (chan, user, nick) of
            (Just c, Nothing, Nothing) -> return $ ChannelTarget c
            (Nothing, Just u, Nothing) -> return $ UserTarget u
            (Nothing, Nothing, Just n) -> return $ NickTarget n
            _ -> fail "Should be either channel, username or nickname"

{- | Construct an arbitrary target. -}
instance Arbitrary Target where
    arbitrary = oneof
        [ ChannelTarget <$> arbitrary
        , UserTarget <$> arbitrary
        , NickTarget <$> arbitrary
        ]

    shrink (ChannelTarget c) = shrink1 ChannelTarget c
    shrink (UserTarget u) = shrink1 UserTarget u
    shrink (NickTarget n) = shrink1 NickTarget n

{- | Define multiple targets as a list of targets. By using the smart
 - constructor, non empty lists are not allowed to be constructed. -}
newtype Targets = Targets [Target] deriving (Show, Read, Eq)

{- | Smart constructor for targets. It will only accept lists of at least one
 - element. -}
targets :: [Target]
    -- ^ Targets source.
    -> Maybe Targets
targets [] = Nothing
targets ts = return $ Targets ts

{- | Get targets from list of targets. -}
getTargets :: Targets
    -- ^ Targets to get targets from.
    -> [Target]
getTargets (Targets ts) = ts

{- | Convert a list of Target's to JSON. -}
instance ToJSON Targets where
    toJSON (Targets ts) = toJSON ts

{- | Parse a list of targets from JSON. -}
instance FromJSON Targets where
    parseJSON = \x -> Targets <$> parseJSON x

{- | Construct an arbitrary non empty list of targets. -}
instance Arbitrary Targets where
    arbitrary = Targets <$> listOf1 arbitrary

    shrink (Targets []) = []
    shrink (Targets [_]) = []
    shrink (Targets (_:ts)) = [Targets ts]
