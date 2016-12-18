module BFS (Node(..), Len, Path(..), Search, bfs) where
import qualified Data.Set as S
import Data.Function (fix)

class Ord n => Node n where
  adjacent :: n -> [n]

type Len = Int
data Path n = Path { pathLen :: Len, pathNode :: n }
              deriving Show
type Search n = [Path n]

adjify :: Node n => Path n -> Search n
adjify (Path l no) = (Path $ succ l) <$> adjacent no

uniquify :: Ord n => Search n -> Search n
uniquify = loop S.empty
  where loop _ [] = []
        loop seen (p:ps)
          | S.member (pathNode p) seen  = loop seen ps
          | otherwise                   = p:(loop (S.insert (pathNode p) seen) ps)

bfsify :: Node n => n -> Search n -> Search n
bfsify n0 = uniquify . ((Path 0 n0):) . concatMap adjify

bfs :: Node n => n -> Search n
bfs = fix . bfsify

data Example = Example Int Int
             deriving (Eq, Ord, Show)
instance Node Example where
  adjacent (Example x y) = [Example (x - 1) y,
                            Example (x + 1) y,
                            Example x (y - 1),
                            Example x (y + 1)]
