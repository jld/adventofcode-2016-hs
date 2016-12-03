module Parse where
import Geom
import Data.Char (isDigit)

parse_turn 'L' = TurnLeft
parse_turn 'R' = TurnRight
parse_turn c = error ("parse_turn: unexpected " ++ show c)

parse_trailing "" e = e
parse_trailing "," e = e
parse_trailing s _ = error ("parse_trailing: unexpected " ++ show s)

parse_numtrail :: String -> Integer
parse_numtrail s =
  let (num, trail) = span isDigit s in
  parse_trailing trail (read num)

parse_cmd (turn:rest) = turnwalk (parse_turn turn) (parse_numtrail rest)

parse_cmds = concat . map parse_cmd . words
