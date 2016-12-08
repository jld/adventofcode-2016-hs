module Screen where
import Parse

type Dim = Int
scrw :: Dim
scrw = 50
scrh :: Dim
scrh = 6

type Coord = (Dim, Dim)
type Pic = Coord -> Bool

blank :: Pic
blank = const False

rect :: Dim -> Dim -> Pic
rect w h (x,y) = x < w && y < h

over :: Pic -> Pic -> Pic
over upper lower xy = upper xy || lower xy

rect_over w h = over $ rect w h

rot_left :: Dim -> Dim -> Coord -> Coord
rot_left row dx (x,y) = (if y == row then (x - dx) `mod` scrw else x, y)

rotright_over :: Dim -> Dim -> Pic -> Pic
rotright_over row dx f = f . rot_left row dx

rot_up :: Dim -> Dim -> Coord -> Coord
rot_up col dy (x,y) = (x, if x == col then (y - dy) `mod` scrh else y)

rotdown_over :: Dim -> Dim -> Pic -> Pic
rotdown_over col dy f = f . rot_up col dy

renderb :: Pic -> [[Bool]]
renderb f = [[f (x,y) | x <- [0..pred scrw]] | y <- [0..pred scrh]]

render :: Pic -> [[Char]]
render = map (map (\b -> if b then '#' else '.')) . renderb

display = putStr . unlines . render

litcount :: Pic -> Int
litcount = length . filter id . concat . renderb

from_op (Rect w h) = rect_over w h
from_op (RotRow row dx) = rotright_over row dx
from_op (RotCol col dy) = rotdown_over col dy

from_ops = foldr (.) id . reverse . map from_op
pic_of_ops = flip from_ops blank
solve = litcount . pic_of_ops
solve_file = fmap solve . parse_file

altsolve = display . pic_of_ops
altsolve_file = (>>= altsolve) . parse_file
