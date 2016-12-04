module Rooms where
import Data.Char (isDigit, isLower, ord, chr)
import Data.List (sort, group, isInfixOf)

namehash =
  map snd . take 5 . sort . map (\l -> (- length l, head l)) . group . sort . filter (/= '-')

data Room = Room String Integer String
            deriving (Eq, Show)

roomsplit s =
  let (enc, rest) = span (\c -> c == '-' || isLower c) s in
  let (num, brac) = span isDigit rest in
  Room enc (read num) brac

room_num (Room _ num _) = num
is_valid (Room enc _ brac) = brac == ("[" ++ (namehash enc) ++ "]")

parse_prob = map roomsplit . lines
parse_file = fmap parse_prob . readFile

solve = sum . map room_num . filter is_valid

from_let c = toInteger $ (ord c) - (ord 'a')
to_let n = chr (fromInteger n + (ord 'a'))
rotnum d n = (d + n) `mod` 26
rotlet d = to_let . rotnum d . from_let
decode1 d '-' = ' '
decode1 d c = rotlet d c

decode (Room enc num _) = map (decode1 num) enc

hunt substr = map room_num . filter (\r -> isInfixOf substr $ decode r)
