module Scramble where
import Control.Monad
import Data.Maybe
import Data.List

rol :: Int -> [a] -> [a]
rol = (liftM2 . liftM2) (++) drop take

ror :: Int -> [a] -> [a]
ror i l = rol ((length l) - i) l

findOrElse :: (Show a, Eq a) => a -> [a] -> Int
findOrElse x = fromMaybe (error ("Not found: " ++ show x)) . elemIndex x

sillyrot i l = ror i' l
  where i' = (1 + i + (if i >= 4 then 1 else 0)) `mod` length l

trisplit i j abc
  | i > j     = trisplit j i abc
  | otherwise = (a, b, c)
  where (a, bc) = splitAt i abc
        (b, c) = splitAt (j - i) bc

extractAt i abc = (b, ac)
  where ac = a ++ c
        (a, b:c) = splitAt i abc

insertAt i b ac = a ++ b:c
  where (a, c) = splitAt i ac

moveFromTo i j = uncurry (insertAt j) . extractAt i

applyBetween f i j abc = a ++ f b ++ c
  where (a, b, c) = trisplit i j abc
revBetween = applyBetween reverse

swapAt i j l = a ++ d:c ++ b:e
  where (a, b:c, d:e) = trisplit i j l
