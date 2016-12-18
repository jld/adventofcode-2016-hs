module Cubes where
import Data.Bits
import BFS (Node(..), bfs)

data Cube = Cube Int Int Int
            deriving (Eq, Ord, Show)

magic (Cube k x y) = k + x*x + 3*x + 2*x*y + y + y*y

isEven n = n `mod` 2 == 0
isOpen = isEven . popCount . magic
boundary (Cube _ x y) = x >= 0 && y >= 0

printable k xx yy =
  [[if isOpen (Cube k x y) then '.' else '#'
   | x <- [0..pred xx]]
  | y <- [0..pred yy]]

step_lf (Cube k x y) = Cube k (x - 1) y
step_rt (Cube k x y) = Cube k (x + 1) y
step_up (Cube k x y) = Cube k x (y - 1)
step_dn (Cube k x y) = Cube k x (y + 1)
steps cu = [step_lf cu, step_rt cu, step_up cu, step_dn cu]

instance Node Cube where
  adjacent = filter isOpen . filter boundary . steps

isAt x y (Cube _ x' y') = x == x' && y == y'

distance' x y = fst . head . filter (isAt x y . snd)
distance x y = distance' x y . bfs

start k = Cube k 1 1
solve x y = distance x y . start
