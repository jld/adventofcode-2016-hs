module Triangle where
import Data.List (sort)

data Tri = Tri Integer Integer Integer
           deriving (Eq, Show)

tri_of_list l =
  case sort l of
   [a, b, c] -> Tri a b c
   _ -> error ("tri_of_list: Length " ++ (show $ length l) ++ " /= 3")

parse_tri = tri_of_list . map read . words
parse_tris = map parse_tri . lines
parse_file = fmap parse_tris . readFile 

is_valid (Tri a b c) = (a + b) > c

solve = length . filter is_valid
