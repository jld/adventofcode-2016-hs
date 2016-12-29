module Tubes where
import BFS
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import Data.Vector.Generic (fromList, (!?))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word

type Tubes = V.Vector (VU.Vector Cell)
type Cell = Word8
type Coord = (Int, Int)

toCell :: Char -> Cell
toCell = toEnum . fromEnum

makeTubes :: String -> Tubes
makeTubes = fromList . map perLine . lines
  where perLine = fromList . map toCell
readTubes = fmap makeTubes . readFile

hall = toCell '.'
wall = toCell '#'
point n = (toCell '0') + (toEnum n)
isPoint = (>= point 0)
pointGet' p = fromEnum p - fromEnum '0'
pointGet p
  | isPoint p = Just $ pointGet' p
  | otherwise = Nothing

tubeGet :: Tubes -> Coord -> Cell
tubeGet t (x,y) = fromMaybe wall $ (!? x) =<< (!? y) t

findCell :: Cell -> Tubes -> Maybe Coord
findCell p = V.ifoldr perLine Nothing
  where perLine y line rest =
          ((\x -> (x, y)) <$> VU.elemIndex p line) <|> rest

findPoint = findCell . point

findPoints :: Tubes -> [Coord]
findPoints = map fromJust . takeWhile isJust . flip map [0..] . flip findPoint

neighbors :: Coord -> [Coord]
neighbors (x,y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
walkable map = filter ((/= wall) . tubeGet map) . neighbors

data TubeNode = TubeNode { nodeMap :: Tubes, nodeLoc :: Coord }

instance Eq TubeNode where
  n0 == n1 = nodeLoc n0 == nodeLoc n1
instance Ord TubeNode where
  n0 `compare` n1 = nodeLoc n0 `compare` nodeLoc n1
instance Show TubeNode where
  showsPrec p (TubeNode _ loc) = showParen (p > 10) stuff
    where stuff = showString "TubeNode _ " . showsPrec 11 loc

instance Node TubeNode where
  adjacent (TubeNode tb xy) = map (TubeNode tb) $ walkable tb xy

bfsTube tb = map (fmap nodeLoc) . bfs . TubeNode tb

data Route a = Route { routeTag :: a, routeLen :: Len }
             deriving (Eq, Ord, Show)

pointsFrom tb xy = do
  path <- bfsTube tb xy
  pt <- maybeToList $ pointGet $ tubeGet tb $ pathNode path
  return $ Route pt $ pathLen path

type TubeMap = [Route (Int, Int)]

tubeMap :: Tubes -> TubeMap
tubeMap tb = do
  (pfrom, xy) <- zip [(0 :: Int)..] $ findPoints tb
  Route pto len <- pointsFrom tb xy
  return $ Route (pfrom, pto) len

-- ugh hax
tmapSize :: TubeMap -> Int
tmapSize = succ . maximum . map (fst . routeTag)

simpleTSP :: TubeMap -> [Route [Int]]
simpleTSP tmp = tsp [0] where
  target = tmapSize tmp
  tsp trail
    | length trail >= target = return $ Route [] 0
    | otherwise = do
        let here = head trail
        Route (pfrom, pto) len <- tmp
        guard $ pfrom == here
        guard $ not $ elem pto trail
        Route restPath restLen <- tsp (pto:trail)
        return $ Route (here:restPath) (len + restLen)

simpleShortest = head . sortOn routeLen . simpleTSP
