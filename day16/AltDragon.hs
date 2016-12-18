module AltDragon where
import Data.Maybe (maybe)

-- FIXME: this was meant to not eat ridiculous amounts of memory, by
-- not constructing a list of all the things, but... somehow it eats
-- memory anyway.
data Dragon = Dragon Int (Int -> Char)

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

check' [] = Just []
check' [_] = Nothing
check' (a:b:cs) = ((ccheck a b):) <$> (check' cs)
check l = maybe l check $ check' l

solve n = check . undrag . dragify n
