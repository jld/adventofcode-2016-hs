module TreeDragon where
import Data.Bits

newtype Checked = Checked Bool
                  deriving (Show, Eq, Ord)
uncheck x _ (Checked False) = x
uncheck _ x (Checked True) = x

instance Monoid Checked where
  mempty = Checked True
  mappend (Checked b0) (Checked b1) = Checked (b0 == b1)

data Tree a = Leaf | Node Integer a (Tree a) a (Tree a)
            deriving Show

tlen (Leaf) = 0
tlen (Node l _ _ _ _) = l

tsum (Leaf) = mempty
tsum (Node _ s _ _ _) = s

build l m r = Node n s l m r where
  n = tlen l + 1 + tlen r
  s = tsum l `mappend` m `mappend` tsum r

fromList :: Monoid a => [a] -> Tree a
fromList = foldr (build Leaf) Leaf

toList Leaf = []
toList (Node _ _ l m r) = (toList l) ++ [m] ++ (toList r)

tmap _ Leaf = Leaf
tmap f (Node n s l m r) = Node n s' l' m' r'
  where s' = f n s
        l' = tmap f l
        m' = f 1 m
        r' = tmap f r

trev Leaf = Leaf
trev (Node n s l m r) = Node n s (trev r) m (trev l)

ttake i Leaf = Leaf
ttake i (Node _ _ l m r)
  | i <= 0             = Leaf
  | i <= tlen l        = ttake i l
  | otherwise          = build l m (ttake (i - 1 - tlen l) r)

tdrop i Leaf = Leaf
tdrop i tr@(Node _ _ l m r)
  | i <= 0            = tr
  | i <= tlen l       = build (tdrop i l) m r
  | otherwise         = tdrop (i - 1 - tlen l) r

clift f (Checked b) = Checked (f b)
cnot i
  | i `mod` 2 == 0 = id
  | otherwise      = clift not
tcnot = tmap cnot

antisense = trev . tcnot
dragIter t = build t (Checked False) (antisense t)

untext = fromList . map (Checked . (/= '0'))
retextL = map (uncheck '0' '1')
retext = retextL . toList

dragonize' n = ttake n . head . dropWhile ((< n) . tlen) . iterate dragIter
dragonize n = dragonize' n . untext

twos n = n .&. (-n)
oddness n = n `div` (twos n)

check' d = do
  let n = tlen d
  let t = twos n
  i <- [0..pred (oddness n)]
  return $ tsum $ ttake t $ tdrop (i * t) d

check = retextL . check'
solve n = check . dragonize n
