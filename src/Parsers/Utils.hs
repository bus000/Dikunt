{-# LANGUAGE FlexibleContexts #-}
module Parsers.Utils
    ( ipV6
    , ipV4
    , hostname
    , username
    , channel
    , nickname
    ) where

import qualified Data.Char as Char
import Data.List (intercalate)
import Numeric (showHex)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

data IPV6 = IPV6 Int Int Int Int Int Int Int Int deriving (Show)

data IPV6Con = Num Int | Zeros deriving (Show, Eq)

{- | Parse an IPV6 addresss. -}
ipV6 :: P.Stream s m Char => P.ParsecT s u m String
ipV6 = do
    cons <- P.try withBridge <|> P.try noBridge
    case ipV6Cons cons of
        Just ip -> return $ ipV6ToString ip
        Nothing -> P.parserFail "Could not parse IPV6."
  where
    withBridge = (\x y -> x ++ [Zeros] ++ y) <$>
        P.endBy1 (Num <$> P.hexnum) (P.char ':') <* P.char ':' <*> noBridge
    noBridge = P.sepBy1 (Num <$> P.hexnum) (P.char ':')

ipV6Cons :: [IPV6Con] -> Maybe IPV6
ipV6Cons cons
    | bridges == 0 && length cons == 8 = fromList $ map (\(Num n) -> n) cons
    | bridges == 1 = fromList $ foldr expand [] cons
    | otherwise = Nothing
  where
    bridges = length . filter (== Zeros) $ cons
    bridgeLen = 8 - length cons + 1

    fromList [a, b, c, d, e, f, g, h] = Just $ IPV6 a b c d e f g h
    fromList _ = Nothing

    expand Zeros ls = replicate bridgeLen 0 ++ ls
    expand (Num n) ls = n:ls

ipV6ToString :: IPV6 -> String
ipV6ToString (IPV6 a b c d e f g h) =
    intercalate ":" $ map (`showHex` "") [a, b, c, d, e, f, g, h]

data IPV4 = IPV4 Int Int Int Int deriving (Show)

{- | Parse an IPV4 address. -}
ipV4 :: P.Stream s m Char => P.ParsecT s u m String
ipV4 = ipV4ToString <$> (IPV4 <$> (P.decimal <* period) <*>
    (P.decimal <* period) <*> (P.decimal <* period) <*> P.decimal)
  where
    period = P.char '.'

ipV4ToString :: IPV4 -> String
ipV4ToString (IPV4 a b c d) = intercalate "." $ map show [a, b, c, d]

{- | Parse hostname which is strings alphanumeric strings (and '-') seperated by
 - dots. -}
hostname :: P.Stream s m Char => P.ParsecT s u m String
hostname = intercalate "." <$> P.sepBy1 shortname (P.char '.')

shortname :: P.Stream s m Char => P.ParsecT s u m String
shortname = do
    short <- P.many1 (P.choice [P.alphaNum, P.char '-'])
    if Char.isAlphaNum (head short) && Char.isAlphaNum (last short)
    then return short
    else P.parserFail "Shortname cannot start or end with '-'"

{- | Parse IRC username. -}
username :: P.Stream s m Char => P.ParsecT s u m String
username = P.many1 (P.noneOf "\0\r\n @%")

{- | Parse an IRC channel name. -}
channel :: P.Stream s m Char => P.ParsecT s u m String
channel = (:) <$> P.choice [P.char '#', P.char '+', P.char '&'] <*>
    P.many1 (P.noneOf "\0\a\r\n ,:")

{- | Parse an IRC nickname. -}
nickname :: P.Stream s m Char => P.ParsecT s u m String
nickname = (:) <$> P.alphaNum <*> P.many (P.choice [P.alphaNum, P.char '-', special])

special :: P.Stream s m Char => P.ParsecT s u m Char
special = P.choice $ map P.char ['[', ']', '\\', '`', '_', '^', '{', '}', '|']
