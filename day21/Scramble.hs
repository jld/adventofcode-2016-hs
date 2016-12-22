module Scramble where
import Control.Monad
import Data.Maybe
import Data.List
import Syntax

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


perform (SwapPos x y) l    = swapAt x y l
perform (SwapLetter x y) l = swapAt (findOrElse x l) (findOrElse y l) l
perform (RotLeft x) l      = rol x l
perform (RotRight x) l     = ror x l
perform (RotBasedOn x) l   = sillyrot (findOrElse x l) l
perform (RevRange x y) l   = revBetween x (succ y) l
perform (Move x y) l       = moveFromTo x y l

invPerform (SwapPos x y) l    = [swapAt x y l]
invPerform (SwapLetter x y) l = [swapAt (findOrElse x l) (findOrElse y l) l]
invPerform (RotLeft x) l      = [ror x l]
invPerform (RotRight x) l     = [rol x l]
invPerform (RotBasedOn x) l   = filter ((== l) . perform (RotBasedOn x)) ls
  where ls = tail $ zipWith (++) (tails l) (inits l)
invPerform (RevRange x y) l   = [revBetween x (succ y) l]
invPerform (Move x y) l       = [moveFromTo y x l]

solve unscr = foldr perform unscr . reverse

unsolve :: String -> [Op] -> [String]
unsolve = foldr ((=<<) . invPerform) . return
