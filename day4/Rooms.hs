module Rooms where
import Data.Char (isDigit, isLower)
import Data.List (sort, group)

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
