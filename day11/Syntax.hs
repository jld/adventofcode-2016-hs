module Syntax where
import Data.Char
import Text.ParserCombinators.ReadP
import Parsify

type Elem = String
data Thing = Chip Elem | Gen Elem
           deriving (Eq, Ord, Show)

elemP = munch1 isLower
chipP = (Chip <$> elemP) <* string "-compatible microchip"
genP = (Gen <$> elemP) <* string " generator"
thingP = chipP +++ genP
aThingP = (string "a " +++ string "an ") *> thingP

thing0P = const [] <$> string "nothing relevant"
thing1P = (:[]) <$> aThingP

thing2P = do
  x <- aThingP
  string " and "
  y <- aThingP
  return [x, y]

thingNP = do
  xs <- many1 (aThingP <* string ", ")
  y <- aThingP
  string ", and "
  z <- aThingP
  return $ xs ++ [y, z]

thingsP = thing0P +++ thing1P +++ thing2P +++ thingNP

floorP ordinal = do
  string "The "
  string ordinal <++ do
    badWord <- munch1 (not . isSpace)
    error ("floorP: Unexpected floor ordinal " ++ show badWord)
  string " floor contains "
  thingsP <* string "."

floorsP = map floorP ["first", "second", "third", "fourth"]

parse_prob = zipWith parsify floorsP . lines
parse_file = fmap parse_prob . readFile
