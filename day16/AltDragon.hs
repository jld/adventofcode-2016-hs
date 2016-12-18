module AltDragon where
import Data.Maybe (maybe)

-- Represent the string as a function instead of a list to allow
-- reversing it without forcing the entire thing to exist in memory.
data Dragon = Dragon Int (Int -> Char)

-- Use chars instead of bools because I got tired of the conversions.
-- Doesn't seem to be a significant perf cost in practice.
cnot '0' = '1'
cnot '1' = '0'
cnot c = error ("cnot: bad boolean " ++ show c)

drag0 l = Dragon (length l) (l !!)
dragiter (Dragon n f) = Dragon (2 * n + 1) f'
  where f' i
          | i < n = f i
          | i == n = '0'
          | otherwise = cnot $ f (2 * n - i)

dragseq = iterate dragiter . drag0
pickdrag n ((Dragon m f):ds)
  | n <= m    = Dragon n f
  | otherwise = pickdrag n ds
dragify n = pickdrag n . dragseq

undrag (Dragon n f) = map f [0..pred n]

ccheck '0' '0' = '1'
ccheck '1' '1' = '1'
ccheck '0' '1' = '0'
ccheck '1' '0' = '0'
ccheck c d = error ("ccheck: bad booleans " ++ show c ++ " " ++ show d)

-- It's tempting to reuse the list-based `check`, but broken: to
-- decide if a list is even or odd length you have to force the entire
-- spine, which means the entire O(n) forest of `ccheck` thunks (or
-- `==` thunks, in the original version) gets allocated before the
-- `check` can yield the first list cell.

check d@(Dragon n f)
  | n `mod` 2 == 0 =
      let f' i = ccheck (f (2 * i)) (f (2 * i + 1)) in
       check $ Dragon (n `div` 2) f'
  | otherwise = d

solve n = undrag . check . dragify n
