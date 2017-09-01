{- |
 - Module      : Types.BotTypes
 - Description : Types and instances representing a bot.
 - Copyright   : (c) Magnus Stavngaard, 2016
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - The main type is the Bot type. The bot represents a connection to a IRC
 - server on a specific channel. The bot also contains a monitor that keeps
 - track of plugin processes. The output and input of plugins can also be found
 - in the monitor. The module also defines types for messages received by the
 - server and messages that can be send to the server. The types are
 - ServerMessage and ClientMessage respectively. Both types can be converted to
 - and from JSON.
 -}
module Types.BotTypes
    ( Bot(..)
    , bot
    , IRCCommand(..)
    , IRCUser(..)
    , UserServer(..)
    , ServerMessage(..)
    , ClientMessage(..)
    , getServerCommand
    , getClientCommand

    -- Bot configuration type and smart constructor.
    , BotConfig(..)
    , botConfig

    -- Nickname type, smart constructor and getter.
    , Nickname
    , nickname
    , getNickname
    -- Hardcoded nicknames.
    , nickservNickname

    -- Channel type, smart constructor and getter.
    , Channel
    , channel
    , getChannel

    -- Servername type, smart constructor and getter.
    , Servername
    , servername
    , getServerName

    -- Username type, smart constructor and getter.
    , Username
    , username
    , getUsername

    -- Hostname type, smart constructor and getter.
    , Hostname
    , hostname
    , getHostname

    -- Targets type, smart constructor and getter.
    , Targets
    , targets
    , getTargets

    -- Message type, smart constructor and getter.
    , Message
    , message
    , getMessage

    -- IRC Target type.
    , Target(..)

    -- Arguments type, smart constructor and getter.
    , Arguments
    , arguments
    , getArguments

    , Password
    , Mode
    , Realname
    ) where

import Types.Internal
import Types.Internal.Nickname
import Types.Internal.Channel
import Types.Internal.Servername
import Types.Internal.Hostname
import Types.Internal.Username
import Types.Internal.Message
import Types.Internal.IRCUser
import Types.Internal.UserServer
import Types.Internal.Target
import Types.Internal.ServerMessage
