module Traps where

txor '.' '.' = '.'
txor '^' '^' = '.'
txor '.' '^' = '^'
txor '^' '.' = '^'
txor a b = error ("txor: bad characters " ++ show a ++ " " ++ show b)

tshl l = tail l ++ "."
tshr l = "." ++ init l

tnext l = zipWith txor (tshl l) (tshr l)

tcount = length . filter (== '^')
ntcount = length . filter (/= '^')

solve n = ntcount . concat . take n . iterate tnext
