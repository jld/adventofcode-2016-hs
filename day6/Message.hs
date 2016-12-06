module Message where
import Data.List (sort, transpose, group)

mostcommon = snd . head . sort . map (\l -> (- length l, head l)) . group . sort
solve = map mostcommon . transpose . lines
