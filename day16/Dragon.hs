module Dragon where
import Data.Bool (bool)
import Data.Maybe (maybe)

antisense = reverse . map not
dragiter d = d ++ [False] ++ antisense d
dragon seed = ds
  where ds = seed:(map dragiter ds) 

fancytake n = take n . head . dropWhile ((< n) . length)

untext = map (/= '0')
retext = map (bool '0' '1')

check' [] = Just []
check' [_] = Nothing
check' (a:b:l) = ((a == b):) <$> check' l
check l = maybe l check $ check' l
