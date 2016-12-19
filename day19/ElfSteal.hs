module ElfSteal where
import qualified Data.Set as S

data Elf = Elf { elfEpoch :: Int, elfNum :: Int }
           deriving (Eq, Ord, Show)
elfInc (Elf ep nu) = Elf (succ ep) nu
elfInit nu = Elf 0 nu

elfSetup n = S.fromList $ map elfInit $ [1..n]

stealNext es =
  let stealer = S.elemAt 0 es
      victim = S.elemAt 1 es
  in S.insert (elfInc stealer) $ S.deleteAt 0 $ S.deleteAt 0 $ es

winnow st = S.elemAt 0 . head . dropWhile ((> 1) . S.size) . iterate st

solve1 = elfNum . winnow stealNext . elfSetup
