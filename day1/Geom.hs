module Geom where
import qualified Data.Set as Set
import Data.List (genericReplicate)

data Loc = Loc Integer Integer
         deriving (Eq, Show, Ord)
data Dir = Dir Integer Integer
         deriving (Eq, Show)
data Turn = TurnLeft | TurnRight
          deriving (Eq, Show)

move (Dir dx dy) (Loc x y) = Loc (x + dx) (y + dy)
rotate TurnLeft (Dir dx dy) = Dir (-dy) dx
rotate TurnRight (Dir dx dy) = Dir dy (-dx)

north = Dir 0 1
east = Dir 1 0
south = Dir 0 (-1)
west = Dir (-1) 0
origin = Loc 0 0

-- Really this should operate on Dir and there should be subtraction, but.
manhattan (Loc dx dy) = (abs dx) + (abs dy)

data Cmd = Turn Turn | Step
           deriving (Eq, Show)
data State = State Loc Dir
           deriving (Eq, Show)

perform Step (State loc dir) = State (move dir loc) dir
perform (Turn t) (State loc dir) = State loc (rotate t dir)

start = State origin north
whereami (State loc dir) = loc

turnwalk :: Turn -> Integer -> [Cmd]
turnwalk t w = (Turn t):(genericReplicate w Step)

solve1 :: [Cmd] -> Integer
solve1 = manhattan . whereami . foldl (flip perform) start

trail = map whereami . scanl (flip perform) start

unstutter :: Eq a => [a] -> [a]
unstutter (x:y:zs) =
  if x == y
  then unstutter (y:zs)
  else x:(unstutter (y:zs))
unstutter l = l

-- TODO: try using hashtables
firstdup :: Ord a => [a] -> Maybe a
firstdup l = loop Set.empty l
  where loop _ [] = Nothing
        loop s (e:l) = if Set.member e s
                       then Just e
                       else loop (Set.insert e s) l

solve2 :: [Cmd] -> Maybe Integer
solve2 = fmap manhattan . firstdup . unstutter . trail
