module Tubes where
import Control.Applicative
import Control.Monad
import Data.Maybe
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

tubeGet :: Tubes -> Coord -> Cell
tubeGet m (x,y) = fromMaybe wall $ (!? x) =<< (!? y) m

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
