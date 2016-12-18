module BFS where
import qualified Data.Set as S
import Data.Function (fix)

class Ord n => Node n where
  adjacent :: n -> [n]

type Len = Int
type Path n = (Len, n)
type Search n = [Path n]

adjify :: Node n => Path n -> Search n
adjify (i, no) = ((,) $ succ i) <$> adjacent no

uniquify :: Ord n => Search n -> Search n
uniquify = loop S.empty
  where loop _ [] = []
        loop seen ((x@(_, no)):xs)
          | S.member no seen  = loop seen xs
          | otherwise         = x:(loop (S.insert no seen) xs)

bfsify :: Node n => n -> Search n -> Search n
bfsify n0 = uniquify . ((0, n0):) . concatMap adjify 

bfs :: Node n => n -> Search n
bfs = fix . bfsify

data Example = Example Int Int
             deriving (Eq, Ord, Show)
instance Node Example where
  adjacent (Example x y) = [Example (x - 1) y,
                            Example (x + 1) y,
                            Example x (y - 1),
                            Example x (y + 1)]
