module Bots where
import qualified Data.Map.Lazy as M
import Parse

wherefrom :: [(Source, Sink)] -> M.Map Sink [Source]
wherefrom = foldr loop M.empty
  where loop (so, si) = M.insertWith (++) si [so]

pairify [a, b] = (a, b)
pairify l = error ("List " ++ show l ++ " is not a pair.")

botsrcs m i = pairify $ M.findWithDefault (error "bot not found") (ToBot i) m

eval1 :: M.Map Sink [Int] -> Source -> Int
eval1 _ (Value n) = n
eval1 msi (BotLo i) = uncurry min $ botsrcs msi i
eval1 msi (BotHi i) = uncurry max $ botsrcs msi i

evaluate :: M.Map Sink [Source] -> M.Map Sink [Int]
evaluate mss = thing
  where thing = M.map (map (eval1 thing)) mss

find_that_does (a,b) = map fst . filter v_ok . M.assocs
  where v_ok (k, v) = v == [a, b] || v == [b, a]

solve = find_that_does (61, 17) . evaluate . wherefrom
solve_file = fmap solve . parse_file

unitize [a] = a
unitize l = error ("List " ++ show l ++ " is not length 1.")

outget i = unitize . M.findWithDefault (error "output not found") (Output i)
altthing m = outget 0 m * outget 1 m * outget 2 m
altsolve = altthing . evaluate . wherefrom
altsolve_file = fmap altsolve . parse_file
