module Josephus where

jos :: Integer -> Integer
jos n
  | n < 1          = error "too low"
  | n == 1         = 1
  | n `mod` 2 == 0 = 2 * (jos (n `div` 2)) - 1
  | otherwise      = 2 * (jos (n `div` 2)) + 1
