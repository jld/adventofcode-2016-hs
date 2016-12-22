module Syntax(Op(..), parse_line, parse_prob, parse_file) where
import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP
import Parsify

data Op = SwapPos Int Int
        | SwapLetter Char Char
        | RotLeft Int
        | RotRight Int
        | RotBasedOn Char
        | RevRange Int Int
        | Move Int Int
        deriving (Eq, Show)

numP :: ReadP Int
numP = read <$> munch1 isDigit

letterP = satisfy isAlpha

swapPosP = do
  string "swap position "
  x <- numP
  string " with position "
  y <- numP
  return $ SwapPos x y

swapLetterP = do
  string "swap letter "
  x <- letterP
  string " with letter "
  y <- letterP
  return $ SwapLetter x y

stepsP = numP <* (string " step" +++ string " steps")

rotLeftP = do
  string "rotate left "
  x <- stepsP
  return $ RotLeft x

rotRightP = do
  string "rotate right "
  x <- stepsP
  return $ RotRight x

rotBasedP = do
  string "rotate based on position of letter "
  x <- letterP
  return $ RotBasedOn x

revRangeP = do
  string "reverse positions "
  x <- numP
  string " through "
  y <- numP
  return $ RevRange x y

moveP = do
  string "move position "
  x <- numP
  string " to position "
  y <- numP
  return $ Move x y

opP = swapPosP +++
      swapLetterP +++
      rotLeftP +++
      rotRightP +++
      rotBasedP +++
      revRangeP +++
      moveP

-- FIXME: should I rethink the "trailing junk" diagnostic?
parse_line = parsify (opP <* eof)
parse_prob = map parse_line . lines
parse_file = fmap parse_prob . readFile
