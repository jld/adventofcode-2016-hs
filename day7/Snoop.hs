module Snoop where

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
