module Main (main) where

import Control.Error.Util (hush)
import Control.Monad (void)
import Data.Aeson (decode)
import Data.Char (isSpace)
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import qualified Types.BotTypes as BT

type User = String
type SearchString = String

data Request
    = Help User
    | GetMundHeld SearchString

main :: IO ()
main = do
    (nick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    requests <- parseRequests nick <$> T.hGetContents stdin

    mapM_ handleRequest requests

handleRequest :: Request -> IO ()
handleRequest (Help botnick) = giveHelp botnick
handleRequest (GetMundHeld searchString) = giveMundHeld searchString

giveHelp :: User -> IO ()
giveHelp botnick = do
    putStrLn $ botnick ++ ": mundheld help - Display this message."
    putStrLn $ botnick ++ ": mundheld <search-string> - Find a mundheld in " ++
        "the database that matches the search string given."

giveMundHeld :: SearchString -> IO ()
giveMundHeld search = case filter (search `isInfixOf`) mundHelds of
    [] -> putStrLn "Jeg fandt ingen mundheld der matchede din søge streng."
    (x:_) -> putStrLn x

{- Sources of mundheld: http://www.udvalgte-ordsprog.dk/danske-ordsprog.htm -}
mundHelds :: [String]
mundHelds =
    [ "Almindelig sund fornuft er ikke så almindelig endda"
    , "De fleste selvmord i Danmark begås med kniv og gaffel"
    , "Det er sundt at leve sjovt, men det er ikke sjovt at leve sundt"
    , "Frisk luft gør lægen fattig"
    , "Fra børn og fulde folk skal man høre sandheden"
    , "Tab og vind med samme sind"
    , "Ingen roser uden torne"
    , "Gammel kærlighed ruster ikke"
    , "Små gryder har også ører"
    , "Hvo intet vover intet vinder"
    , "Sælg ikke skindet, før bjørnen er skudt"
    , "Hvad du ikke ved af, har du ikke ondt af"
    , "Tyv tror hver mand stjæler"
    , "Uden mad og drikke duer helten ikke"
    , "Man skal kravle, før man kan gå"
    , "Tale er sølv, tavshed er guld"
    , "Blind høne kan også finde korn"
    , "Den der ler sidst, ler bedst"
    , "Der er ikke rust på et nyttigt redskab"
    , "Tomme tønder buldrer mest"
    , "Når katten er ude, danser musene på bordet"
    , "Gå ikke over åen efter vand"
    , "Hop ikke over hvor gærdet er lavest"
    , "Hvor der handles, der spildes"
    , "Krage søger mage"
    , "Lige børn leger bedst"
    , "Man skal smede, mens jernet er varmt"
    , "Den som jager to harer, får ingen"
    , "Mange bække små gør en stor å"
    , "For mange kokke fordærver maden"
    , "Nogle bygger læhegn, mens andre bygger vindmøller"
    , "Saml appetit ude, spis hjemme"
    , "Slå ikke større brød op, end du kan bage"
    , "Den der graver en grav for andre, falder selv deri"
    , "Kært barn har mange navne"
    , "Den der kommer først til mølle, får først malet"
    , "Sæt ikke dit lys under en skæppe"
    , "Tro kan flytte bjerge"
    , "Man skal ikke kaste med sten, når man selv bor i et glashus"
    , "Man kan ikke både blæse og have mel i munden"
    , "Mod dumhed kæmper selv guderne forgæves"
    , "Livet er som en barneskjorte, alt for kort og utrolig beskidt"
    , "Klog hund tisser ikke mod vinden"
    , "Bedre sent end aldrig"
    , "Den sidste idiot, er ikke født endnu"
    ]

parseRequests :: User -> T.Text -> [Request]
parseRequests botnick =
    mapMaybe getRequest . mapMaybe (decode . T.encodeUtf8) . T.lines
  where
    getRequest (BT.ServerPrivMsg _ _ msg) =
        hush $ P.parse (request botnick) "(stdin)" (BT.getMessage msg)
    getRequest _ = Nothing

request :: User -> P.Parsec String () Request
request botnick = do
    void $ token (P.string $ botnick ++ ":")
    void $ token (P.string "mundheld")
    req <- P.try (helpRequest botnick) <|> P.try mundHeldRequest
    P.eof

    return req

helpRequest :: User -> P.Parsec String () Request
helpRequest botnick = token (P.string "help") *> return (Help botnick)

mundHeldRequest :: P.Parsec String () Request
mundHeldRequest = GetMundHeld . trim <$> P.many1 (P.noneOf "\r\n")

token :: P.Parsec String () a -> P.Parsec String () a
token tok = P.spaces *> tok <* P.spaces

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
