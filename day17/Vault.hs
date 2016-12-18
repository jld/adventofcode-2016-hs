module Vault where
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Crypto.Hash.MD5 as M
import BFS

data Jaunt = Jaunt Int Int B.ByteString
             deriving (Eq, Ord, Show)
start s = Jaunt 0 0 (BC.pack s)

tackOn k = B.snoc k . toEnum . fromEnum

jup (Jaunt x y k) = Jaunt x (y - 1) (tackOn k 'U')
jdn (Jaunt x y k) = Jaunt x (y + 1) (tackOn k 'D')
jlf (Jaunt x y k) = Jaunt (x - 1) y (tackOn k 'L')
jrt (Jaunt x y k) = Jaunt (x + 1) y (tackOn k 'R')

between l h n = n >= l && n <= h
boundary (Jaunt x y _) = between 0 3 x && between 0 3 y

nybbles [] = []
nybbles (b:bs) = (b `shiftR` 4):(b .&. 0x0f):(nybbles bs)

doors' = map (>= 11) . take 4 . nybbles . B.unpack
doors (Jaunt _ _ k) = doors' $ M.hash k
ways j = zip [jup j, jdn j, jlf j, jrt j] (doors j)

realAdj = map fst . filter snd . filter (boundary . fst) . ways

instance Node Jaunt where
  adjacent j
    | final j   = []
    | otherwise = realAdj j

final j@(Jaunt x y _) = (x,y) == (3,3)

solveish = head . filter final . map pathNode . bfs . start

altsolve = pathLen . last . filter (final . pathNode) . bfs . start
