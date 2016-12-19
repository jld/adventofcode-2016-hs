module Parsify where
import Text.ParserCombinators.ReadP

parsify p s =
  case readP_to_S p s of
    [(v, "")] -> v
    [(_, junk)] ->
      error ("Input " ++ show s ++ " had trailing junk " ++ show junk)
    [] ->
      error ("Input " ++ show s ++ " had no parse")
    _ ->
      error ("Input " ++ show s ++ " was ambiguous which shouldn't happen")
