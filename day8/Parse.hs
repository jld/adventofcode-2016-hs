module Parse where
import Data.Char (isDigit)

-- "Borrowed" from the day7 hypernet parser.
-- (Note: depends on valid lines always ending with a number.)
explode :: String -> [(String, Int)]
explode "" = []
explode s =
  let (l,mr) = span (not . isDigit) s in
  let (m,r) = span isDigit mr in
  (l,read m):(explode r)

data Op = Rect Int Int
        | RotRow Int Int
        | RotCol Int Int
        deriving (Eq, Show)

parse' [("rect ", w), ("x", h)] = Rect w h
parse' [("rotate row y=", row), (" by ", dx)] = RotRow row dx
parse' [("rotate column x=", col), (" by ", dy)] = RotCol col dy
parse' thing = error $ "Syntax error: " ++ show thing

parse = parse' . explode

parse_prob = map parse . lines
parse_file = fmap parse_prob . readFile
