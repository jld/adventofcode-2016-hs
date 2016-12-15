module Parse where
import Discs
import Data.Char
import Text.ParserCombinators.ReadP

numP :: ReadP Integer
numP = fmap read $ munch1 isDigit

discP = do
  string "Disc #"
  disc <- numP
  string " has "
  period <- numP
  string " positions; at time=0, it is at position "
  pos0 <- numP
  string "."
  -- I might need to factor this out later, but for now:
  return $ make_seq period (- (disc + pos0))

-- FIXME: stop copypasting this
parsify p s =
  case readP_to_S p s of
    [(v, "")] -> v
    [(_, junk)] ->
      error ("Input " ++ show s ++ " had trailing junk " ++ show junk)
    [] ->
      error ("Input " ++ show s ++ " had no parse")
    _ ->
      error ("Input " ++ show s ++ " was ambiguous which shouldn't happen")

parse_line = parsify discP
-- Also these are getting a little repetitive:
parse_prob = map parse_line . lines
parse_file = fmap parse_prob . readFile
