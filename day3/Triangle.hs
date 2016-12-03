module Triangle where
import Data.List (sort, transpose)

data Tri = Tri Integer Integer Integer
           deriving (Eq, Show)

tri_of_list l =
  case sort l of
   [a, b, c] -> Tri a b c
   _ -> error ("tri_of_list: Length " ++ (show $ length l) ++ " /= 3")

tri_of_words = tri_of_list . map read
parse_tri = tri_of_words . words
parse_tris = map parse_tri . lines
fileize f = fmap f . readFile
parse_file = fileize parse_tris

is_valid (Tri a b c) = (a + b) > c

solve = length . filter is_valid

explode = map words . lines
mangle (a:b:c:ds) = [a,b,c]:(mangle ds)
mangle [] = []
twirl = concat . map transpose . mangle

altparse_tris = map tri_of_words . twirl . explode
altparse_file = fileize altparse_tris
