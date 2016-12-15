module Discs where
import Data.List

data Sequence = Sequence { seq_mod :: Integer, seq_off :: Integer }
              deriving (Eq, Show)

make_seq sm so = Sequence { seq_mod = sm, seq_off = so `mod` sm }
unit_seq = Sequence { seq_mod = 1, seq_off = 0 }

in_seq s n = n `mod` (seq_mod s) == seq_off s
index_seq s = (+ (seq_off s)) . (* (seq_mod s))
generate s = map (index_seq s) [0..]

-- The name "intersect" is already spoken for, so...
polyrhythm sa sb = Sequence {
  seq_mod = lcm (seq_mod sa) (seq_mod sb),
  seq_off = head $ filter (in_seq sa) $ genericTake (seq_mod sa) (generate sb)
}

multirhythm :: [Sequence] -> Sequence
multirhythm = foldr polyrhythm unit_seq
solve = seq_off . multirhythm
