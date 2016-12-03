module KeyPad where

data Dir = Up | Dn | Lf | Rt
         deriving (Eq, Show)

parse_dir 'U' = Up
parse_dir 'D' = Dn
parse_dir 'L' = Lf
parse_dir 'R' = Rt
parse_dir x = error ("parse_dir: unrecognized " ++ show x)
parse_dirs = map parse_dir
parse_prob = map parse_dirs . words
parse_file path = fmap parse_prob $ readFile path

data Zipper a = Zipper [a] a [a]
fwd (Zipper before here after) = Zipper (here:before) (head after) (tail after)
back (Zipper before here after) = Zipper (tail before) (head before) (here:after)
zget (Zipper _ here _) = here

instance Functor Zipper where
  fmap f (Zipper before here after) = Zipper (map f before) (f here) (map f after)

mkzip fill before here after = Zipper (reverse before ++ repeat fill) here (after ++ repeat fill)
mkline = mkzip ' '
blankline = mkline "" ' ' ""
mkpad = mkzip blankline

type Pad = Zipper (Zipper Char)
padget = zget . zget
shift Up = back
shift Dn = fwd
shift Lf = fmap back
shift Rt = fmap fwd
move dir pad =
  if padget newpad == ' ' then pad else newpad
  where newpad = shift dir pad

key9 = (mkpad
        [mkline "1" '2' "3"]
        (mkline "4" '5' "6")
        [mkline "7" '8' "9"])

key13 = (mkpad
         [mkline "" ' ' " 1",
          mkline "" ' ' "234"]
         (mkline "" '5' "6789")
         [mkline "" ' ' "ABC",
          mkline "" ' ' " D"])

moves :: [Dir] -> Pad -> Pad
moves = flip (foldl (flip move))

solve :: Pad -> [[Dir]] -> String
solve pad = map padget . tail . scanl (flip moves) pad
