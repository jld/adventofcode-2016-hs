module Snoop where
import Data.Tuple (swap)
import qualified Data.Set as S

-- FIXME: there's probably a nicer way to pattern-match this
has_abba (a:b:c:d:es) = (a == d && b == c && a /= b) || has_abba (b:c:d:es)
has_abba _ = False

xtail (h:t) = t
xtail [] = []

unbracket "" = []
unbracket s =
  let (l,mr) = span (/= '[') s in
  let (m,r) = span (/= ']') $ xtail mr in
  (l,m):(unbracket $ xtail r)

snoopable (sns,hns) = any has_abba sns && not (any has_abba hns)
brunzip = unzip . unbracket
check = snoopable . brunzip
solve = length . filter check . lines

-- FIXME: again, there's probably prettier pattern-matching
abas (a:b:c:ds) = thing ++ abas (b:c:ds)
  where thing = if a == c && a /= b then [(a, b)] else []
abas _ = []
babs = map swap . abas

setify = foldr S.insert S.empty

-- FIXME: surely this is factorable
listens (sns,hns) = S.intersection aba_set bab_set
  where aba_set = setify $ concat $ map abas sns
        bab_set = setify $ concat $ map babs hns
listenable = not . S.null . listens
altcheck = listenable . brunzip
altsolve = length . filter altcheck . lines
