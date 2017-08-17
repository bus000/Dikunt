{- |
 - Module      : Types.Internal.UserServer
 - Description : Representation of an IRC user@server target.
 - Copyright   : (c) Magnus Stavngaard, 2017
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - A Userserver is used as a target for some IRC operations.
 -}
{-# LANGUAGE OverloadedStrings #-}
module Types.Internal.UserServer where

import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)
import Types.Internal.Hostname (Hostname(..))
import Types.Internal.Servername (Servername(..))
import Types.Internal.Username (Username(..))
import Data.Aeson (ToJSON(..), FromJSON(..), withObject, (.=), (.:), (.:?), object)
import Utils (shrink3)

{- | The UserServer type. -}
data UserServer = UserServer Username (Maybe Hostname) (Maybe Servername)
  deriving (Show, Read, Eq)

{- | Convert a UserServer to JSON. -}
instance ToJSON UserServer where
    toJSON (UserServer user Nothing Nothing) = object
        [ "username" .= user
        ]
    toJSON (UserServer user Nothing (Just server)) = object
        [ "username" .= user
        , "servername" .= server
        ]
    toJSON (UserServer user (Just host) Nothing) = object
        [ "username" .= user
        , "hostname" .= host
        ]
    toJSON (UserServer user (Just host) (Just server)) = object
        [ "username" .= user
        , "hostname" .= host
        , "servername" .= server
        ]

{- | Parse a UserServer from Json. -}
instance FromJSON UserServer where
    parseJSON = withObject "IRCUser" $ \o ->
        UserServer <$> o .: "username" <*> o .:? "hostname" <*> o .:? "servername"

{- | Construct an arbitrary UserServer. -}
instance Arbitrary UserServer where
    arbitrary = UserServer <$> arbitrary <*> arbitrary <*> arbitrary

    shrink (UserServer user host serv) = shrink3 UserServer user host serv
