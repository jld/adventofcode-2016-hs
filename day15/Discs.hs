module Discs where
import Data.List

data Sequence = Sequence { seq_mod :: Integer, seq_off :: Integer }
              deriving (Eq, Show)

unit_seq = Sequence { seq_mod = 1, seq_off = 0 }

in_seq s n = n `mod` (seq_mod s) == seq_off s
index_seq s = (+ (seq_off s)) . (* (seq_mod s))
generate s = map (index_seq s) [0..]

-- The name "intersect" is already spoken for, so...
polyrhythm sa sb = Sequence {
  seq_mod = lcm (seq_mod sa) (seq_mod sb),
  seq_off = head $ filter (in_seq sb) $ genericTake (seq_mod sb) (generate sa)
}

delay_seq x s = Sequence {
  seq_mod = seq_mod s,
  seq_off = (x + (seq_off s)) `mod` (seq_mod s)
}

multirhythm = foldr polyrhythm unit_seq
solveL = seq_off . multirhythm . zipWith delay_seq [1..]
solve = seq_off . multirhythm . map (uncurry delay_seq)
