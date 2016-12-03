module Geom where

data Loc = Loc Integer Integer
         deriving (Eq, Show)
data Dir = Dir Integer Integer
         deriving (Eq, Show)

move (Dir dx dy) dist (Loc x y) = Loc (x + dist * dx) (y + dist * dy)
turn_left (Dir dx dy) = Dir (-dy) dx
turn_right (Dir dx dy) = Dir dy (-dx)

north = Dir 0 1
east = Dir 1 0
south = Dir 0 (-1)
west = Dir (-1) 0
origin = Loc 0 0

-- Really this should operate on Dir and there should be subtraction, but.
manhattan (Loc dx dy) = (abs dx) + (abs dy)

data Cmd = Cmd (Dir -> Dir) Integer
data State = State Loc Dir
           deriving (Eq, Show)
state_rot turn (State loc dir) = State loc (turn dir)
state_mov dist (State loc dir) = State (move dir dist loc) dir
walk (Cmd turn dist) = state_mov dist . state_rot turn

start = State origin north
whereami (State loc dir) = loc

solve :: [Cmd] -> Integer
solve = manhattan . whereami . foldl (flip walk) start
