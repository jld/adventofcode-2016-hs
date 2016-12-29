module BFS (Node(..), Len, Path(..), Search, bfs) where
import Control.Monad.State.Lazy
import qualified Data.Set as S

class Ord n => Node n where
  adjacent :: n -> [n]

type Len = Int
data Path n = Path { pathLen :: Len, pathNode :: n }
              deriving Show
type Search n = [Path n]

instance Functor Path where
  fmap f (Path l n) = Path l (f n)

adjify :: Node n => Search n -> Search n
adjify = concatMap explode
explode (Path l no) = (Path $ succ l) <$> adjacent no

uniquify :: Ord n => Search n -> State (S.Set n) (Search n)
uniquify [] = return []
uniquify (p:ps) = do
  let node = pathNode p
  seen <- get
  if S.member node seen
    then uniquify ps
    else do
    modify $ S.insert node
    (p:) <$> uniquify ps

uniqueRun = flip evalState S.empty

iterateM :: Monad m => (a -> m a) -> a -> m [a]
iterateM f x = f x >>= (fmap (x:) . iterateM f)

frontiers :: Node n => n -> [Search n]
frontiers n0 = uniqueRun (uniquify [Path 0 n0] >>= iterateM (uniquify . adjify))

bfs :: Node n => n -> Search n
bfs = concat . takeWhile ((> 0) . length) . frontiers

data Example = Example Int Int
             deriving (Eq, Ord, Show)
instance Node Example where
  adjacent (Example x y) = [Example (x - 1) y,
                            Example (x + 1) y,
                            Example x (y - 1),
                            Example x (y + 1)]
