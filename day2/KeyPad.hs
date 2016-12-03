module KeyPad where

newtype Sat3 = Sat3 Int
             deriving (Eq, Ord, Show)
inc (Sat3 n) = Sat3 (min 2 (n+1))
dec (Sat3 n) = Sat3 (max 0 (n-1))

data Button = Button Sat3 Sat3
            deriving (Eq, Show)

button_val (Button (Sat3 x) (Sat3 y)) = 1 + x + 3 * y
five = Button (Sat3 1) (Sat3 1)

data Dir = Up | Dn | Lf | Rt
         deriving (Eq, Show)
move Up (Button x y) = Button x (dec y)
move Dn (Button x y) = Button x (inc y)
move Lf (Button x y) = Button (dec x) y
move Rt (Button x y) = Button (inc x) y


parse_dir 'U' = Up
parse_dir 'D' = Dn
parse_dir 'L' = Lf
parse_dir 'R' = Rt
parse_dir x = error ("parse_dir: unrecognized " ++ show x)
parse_dirs = map parse_dir
parse_prob = map parse_dirs . words
parse_file path = fmap parse_prob $ readFile path

moves :: [Dir] -> Button -> Button
moves = flip (foldl (flip move))

lsolve1 :: [[Dir]] -> [Int]
lsolve1 = map button_val . tail . scanl (flip moves) five

solve1 :: [[Dir]] -> String
solve1 = concat . map show . lsolve1
