module ElfSteal where
import qualified Data.Set as S

data Elf = Elf { elfEpoch :: Int, elfNum :: Int }
           deriving (Eq, Ord, Show)
elfInc (Elf ep nu) = Elf (succ ep) nu
elfInit nu = Elf 0 nu

elfSetup n = S.fromList $ map elfInit $ [1..n]

genSteal vf es =
  let stealer = S.elemAt 0 es
      vicidx = vf es
      victim = S.elemAt vicidx es
      rest = S.deleteAt 0 $ S.deleteAt vicidx es
  in S.insert (elfInc stealer) rest

stealNext = genSteal (const 1)
stealAcross = genSteal ((`div` 2) . S.size)

winnow st = S.elemAt 0 . head . dropWhile ((> 1) . S.size) . iterate st

solve1 = elfNum . winnow stealNext . elfSetup
solve2 = elfNum . winnow stealAcross . elfSetup
