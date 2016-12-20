module Fire where
import Data.Word
import Data.List

type Addr = Word32
data Range = Range { rangeMin :: Addr, rangeMax :: Addr }
             deriving (Eq, Ord, Show)

-- Assumes arguments in order.
rmerge1 (Range a b) (Range c d)
  | b >= c || succ b == c = Just $ Range a (b `max` d)
  | otherwise             = Nothing

-- Likewise.
rmerge :: [Range] -> [Range]
rmerge (r0:r1:rs)
  | Just r <- rmerge1 r0 r1 = rmerge (r:rs)
  | otherwise              = r0:(rmerge (r1:rs))
rmerge rs = rs

simplify = rmerge . sort

solve' = succ . rangeMax . head
solve = solve' . simplify

rangeLen (Range a b) = succ $ toInteger (b - a)
numBlocked = sum . map rangeLen
numAllowed = (0x100000000 -) . numBlocked

altsolve = numAllowed . simplify

parse_range s = Range a b
  where [a,b] = map read $ words $ map tr s
        tr '-' = ' '
        tr c   = c
parse_prob = map parse_range . lines
parse_file = fmap parse_prob . readFile
