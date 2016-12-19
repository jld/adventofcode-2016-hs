module Parse (Source(..), Sink(..),
              parse_line, parse_prob, parse_file) where
import Text.ParserCombinators.ReadP
import Data.Char
import Parsify

data Source = Value Int
            | BotLo Int
            | BotHi Int
            deriving (Eq, Ord, Show)

data Sink = Output Int
          | ToBot Int
          deriving (Eq, Ord, Show)

num :: ReadP Int
num = fmap read $ munch1 isDigit

bot = string "bot " >> num
output = string "output " >> num
value = string "value " >> num

sink = fmap Output output +++ fmap ToBot bot

goes = do
  so <- fmap Value value
  string " goes to "
  si <- sink
  return [(so, si)]

gives = do
  so <- bot
  string " gives low to "
  sil <- sink
  string " and high to "
  sih <- sink
  return [(BotLo so, sil), (BotHi so, sih)]

line = goes +++ gives

parse_line = parsify line
parse_prob = concat . map parse_line . lines
parse_file = fmap parse_prob . readFile
