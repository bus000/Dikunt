{- |
 - Module      : IRCParser.Layer1Impl
 - Description : Parses IRC messages.
 - Copyright   : (c) Magnus Stavngaard, 2016
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - Implementation of parsing IRC messages from a string to an IRCMessage the
 - general structure of an IRC message is
 -
 -     message = [ ":" prefix SPACE ] command [ params ] crlf.
 -
 - See https://tools.ietf.org/html/rfc2812 for details.
 -}
module IRCParser.Layer1Impl where

import qualified BotTypes as BT
import Control.Applicative ((<$>))
import Text.ParserCombinators.ReadP
    ( ReadP
    , readP_to_S
    , many
    , eof
    , char
    , munch1
    , many1
    , pfail
    , option
    , between
    , string
    , (+++)
    , optional
    , sepBy1
    , satisfy
    )
import Data.Char (isAlpha, isDigit)
import Data.List (intercalate, nub)
import Safe (readMay)

{- | Parse a IRC message from a string. -}
parseIRCMessage :: String
    -- ^ String to parse message from.
    -> Maybe BT.IRCMessage
parseIRCMessage str = case nub (readP_to_S parseMessage str) of
    [(message, "")] -> Just message
    _ -> Nothing

{- | Parse an IRC message of the form
 -     [ ":" prefix SPACE ] command [ params ] crlf -}
parseMessage :: ReadP BT.IRCMessage
parseMessage = do
    prefix <- option Nothing (Just <$> between (char ':') (char ' ') parsePrefix)
    command <- parseCommand
    (params, trailing) <- parseParams

    _ <- string "\r\n"
    eof

    return $ BT.IRCMessage prefix command params trailing

{- | Parse an IRC prefix of the form
 -     servername / ( nickname [ [ "!" user ] "@" host ] ). -}
parsePrefix :: ReadP BT.IRCPrefix
parsePrefix = parseServername +++ parseNicknamePrefix

{- | Parse an IRC command of the form
 -     1*letter / 3digit. -}
parseCommand :: ReadP BT.IRCCommand
parseCommand = parseTextCommand +++ parseNumericCommand
  where
    parseTextCommand = do
        command <- many1 parseLetter
        case readMay command of
            Just com -> return com
            Nothing -> pfail
    parseNumericCommand = do
        d1 <- parseDigit
        d2 <- parseDigit
        d3 <- parseDigit

        case readMay [d1, d2, d3] of
            Just num -> return $ BT.NUMCOM num
            Nothing -> pfail

{- | Parse IRC parameters one of the two forms
 -     *14( SPACE middle ) [ SPACE ":" trailing ],
 -     14( SPACE middle ) [ SPACE [ ":" ] trailing ]. -}
parseParams :: ReadP ([String], Maybe String)
parseParams = form1 +++ form2
  where
    form1 = do
        params <- many (char ' ' >> parseMiddle)
        trailing <- option Nothing (fmap Just $ string " :" >> parseTrailing)

        if length params <= 14
        then return (params, trailing)
        else pfail
    form2 = do
        params <- many (char ' ' >> parseMiddle)
        trailing <- option Nothing (fmap Just $ char ' ' >> optional (char ':')
            >> parseTrailing)

        if length params == 14
        then return (params, trailing)
        else pfail

{- | Parse single IRC parameter of the form
 -     nospcrlfcl *( ":" / nospcrlfcl ). -}
parseMiddle :: ReadP String
parseMiddle = do
    c1 <- parseNospcrlfcl
    rest <- many (char ':' +++ parseNospcrlfcl)

    return $ c1:rest

{- | Parse the trailing part of an IRC message of the form
 -     *( ":" / " " / nospcrlfcl ). -}
parseTrailing :: ReadP String
parseTrailing = many (char ':' +++ char ' ' +++ parseNospcrlfcl)

{- | Parse an IRC server name of the form
 -     servername = hostname. -}
parseServername :: ReadP BT.IRCPrefix
parseServername = do
    servername <- parseHostname
    return $ BT.ServernamePrefix servername

{- | Parse an IRC nickname prefix of the form
 -     nickname [ [ "!" user ] "@" host ]. -}
parseNicknamePrefix :: ReadP BT.IRCPrefix
parseNicknamePrefix = do
    nick <- parseNickname
    user <- option Nothing (fmap Just $ char '!' >> parseUser)
    host <- option Nothing (fmap Just $ char '@' >> parseHost)

    return $ BT.NicknamePrefix (BT.IRCUser nick user host)

{- | Parse an IRC host of the form
 -     hostname / hostaddr. -}
parseHost :: ReadP String
parseHost = parseHostname +++ parseHostaddr

{- | Parse an IRC hostname of the form
 -     hostname = shortname *( "." shortname ). -}
parseHostname :: ReadP String
parseHostname = do
    names <- sepBy1 parseShortname (char '.')
    return $ intercalate "." names

{- | Parse an IRC shortname of the form
 -     ( letter / digit ) *( letter / digit / "-" ) *( letter / digit ). -}
parseShortname :: ReadP String
parseShortname = do
    str <- many (parseLetter +++ parseDigit +++ char '-')

    if not (null str) && last str /= '-' && head str /= '-'
    then return str
    else pfail

    {-return $ [c1] ++ middle ++ [c2]-}

{- | Parse an IRC host address of the form
 -     ip4addr / ip6addr. -}
parseHostaddr :: ReadP String
parseHostaddr = parseIP4addr +++ parseIP6addr

{- | Parse an IPv4 address of the form
 -     1*3digit "." 1*3digit "." 1*3digit "." 1*3digit. -}
parseIP4addr :: ReadP String
parseIP4addr = do
    d1 <- many1 parseDigit
    _ <- char '.'
    d2 <- many1 parseDigit
    _ <- char '.'
    d3 <- many1 parseDigit
    _ <- char '.'
    d4 <- many1 parseDigit

    if length d1 <= 3 && length d2 <= 3 && length d3 <= 3 && length d4 <= 3
    then return $ intercalate "." [d1, d2, d3, d4]
    else pfail

{- | Parse an IPv6 address one of the two forms
 -     1*hexdigit 7( ":" 1*hexdigit ),
 -     "0:0:0:0:0:" ( "0" / "FFFF" ) ":" ip4addr. -}
parseIP6addr :: ReadP String
parseIP6addr = form1 +++ form2
  where
    form1 = do
        hexdigits <- sepBy1 (many1 parseHexdigit) (char ':')
        if length hexdigits == 8
        then return $ intercalate ":" hexdigits
        else pfail
    form2 = do
        prefix <- string "0:0:0:0:0:0:" +++ string "0:0:0:0:0:FFFF:"
        ipv4End <- parseIP4addr

        return $ prefix ++ ipv4End

-- TODO: It seems like freenode actually allow nicknames to be more than 9
-- characters long. Since we still want to conform to the standard the default
-- nickname max length should still be 9 but it should be possible to configure
-- the max length to be something else. This will probably require making the
-- parser use the Reader monad with some configuration state.
{- | Parse an IRC nickname of the form
 -     ( letter / special ) *8( letter / digit / special / "-" ). -}
parseNickname :: ReadP String
parseNickname = do
    c1 <- parseLetter +++ parseSpecial
    rest <- many (parseLetter +++ parseDigit +++ parseSpecial +++ char '-')

    if length rest <= 8 then return (c1:rest) else pfail

{- | Parse an IRC user which is any non empty sequence of characters that are
 - not NUL, CR, LF, " " and "@". -}
parseUser :: ReadP String
parseUser = munch1 (\c -> c `notElem` ['\0', '\r', '\n', ' ', '@'])

{- | Parse an IRC letter which is any of the character a-Z. -}
parseLetter :: ReadP Char
parseLetter = satisfy isAlpha

{- | Parse an IRC digit which is any of the characters 0-9. -}
parseDigit :: ReadP Char
parseDigit = satisfy isDigit

{- | Parse an IRC special symbol which is any of the characters ["[", "]", "\",
 - "`", "_", "^", "{", "|", "}"]. -}
parseSpecial :: ReadP Char
parseSpecial = satisfy (`elem` ['[', ']', '\\', '`', '_', '^', '{', '|', '}'])

{- | Parse an IRC hex digit which is of the form
 -     digit / "A" / "B" / "C" / "D" / "E" / "F". -}
parseHexdigit :: ReadP Char
parseHexdigit = parseDigit +++ satisfy (`elem` ['A', 'B', 'C', 'D', 'E', 'F'])

{- | Parse an IRC nospcrlfcl message which is any character not NUL, CR, LF,
 - " " and ":". -}
parseNospcrlfcl :: ReadP Char
parseNospcrlfcl = satisfy (\c -> c `notElem` ['\0', '\r', '\n', ' ', ':'])
