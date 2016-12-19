module Nukes where
import Data.Bits
import Data.Word
import BFS
import SymTab
import qualified Syntax

-- Hopefully I never need to change these, but:

type ElemBits = Word8
type ThingBits = Word16
type StuffBits = Word64
typeShift = 8 :: Int
floorShift = 16 :: Int
allThings = 0xffff :: ThingBits
numFloors = 4 :: Int

-- fancy ints

newtype Elem = Elem Int
             deriving (Eq, Ord, Show)
elemGet (Elem i) = i
safeElem i
  | i < typeShift = Elem i
  | otherwise     = error ("Element index too large: " ++ show i)          

newtype Thing = Thing Int
              deriving (Eq, Ord, Show)
thingGet (Thing i) = i
chipThing (Elem i) = Thing i
genThing (Elem i) = Thing (i + typeShift)

newtype Floor = Floor Int
              deriving (Eq, Ord, Show)
floorGet (Floor i) = i
safeFloor i
  | i < numFloors = Floor i
  | otherwise     = error ("Floor number too large: " ++ show i)
groundFloor = Floor 0
topFloor = Floor (pred numFloors)
allFloors = map Floor [0..pred numFloors]
incFloor f
  | f < topFloor = Just $ Floor $ succ $ floorGet f
  | otherwise    = Nothing
decFloor f
  | f > groundFloor = Just $ Floor $ pred $ floorGet f
  | otherwise       = Nothing
adjFloors f = [f' | Just f' <- [incFloor f, decFloor f]]

-- fancy sets

newtype Elems = Elems ElemBits
              deriving (Eq, Ord, Show)
newtype Things = Things ThingBits
              deriving (Eq, Ord, Show)
newtype Stuff = Stuff StuffBits
              deriving (Eq, Ord, Show)

bitCast :: (Integral a, Integral b) => a -> b
bitCast = fromInteger . toInteger

emptyElems = Elems 0

elemsNull (Elems 0) = True
elemsNull _         = False
elemsSub (Elems bl) (Elems br) = Elems (bl .&. complement br)

thingsChips (Things b) = Elems $ bitCast b
thingsGens (Things b) = Elems $ bitCast $ shiftR b typeShift

emptyThings = Things 0
oneThing (Thing t) = Things (bit t)
twoThings (Thing ta) (Thing tb) = Things (bit ta .|. bit tb)

thingsContain (Things b) (Thing i) = b `testBit` i

thingsAdd (Things toAdd) (Things b)
  | b .&. toAdd == 0 = Things (b .|. toAdd) 
  | otherwise        = error ("Set " ++ show b ++ " already has things in " ++ show toAdd)
thingsRem (Things toRem) (Things b)
  | b .&. toRem == toRem = Things (b `xor` toRem)
  | otherwise            = error ("Set " ++ show b ++ " lacks things in " ++ show toRem)

thingsAdd1 = thingsAdd . oneThing
thingsFromList :: [Thing] -> Things
thingsFromList = foldr thingsAdd1 emptyThings
thingsToList ts = filter (thingsContain ts) $ map Thing [0..pred floorShift]

emptyStuff = Stuff 0
thingsOnFloor (Floor i) (Things b) =
  Stuff $ (bitCast b) `shiftL` (i * floorShift)
stuffGet (Floor i) (Stuff b) =
  Things $ bitCast $ b `shiftR` (i * floorShift)

stuffNull (Stuff 0) = True
stuffNull _         = False

stuffAdd (Stuff toAdd) (Stuff b)
  | b .&. toAdd == 0 = Stuff (b .|. toAdd) 
  | otherwise        = error ("Set " ++ show b ++ " already has stuff in " ++ show toAdd)
stuffRem (Stuff toRem) (Stuff b)
  | b .&. toRem == toRem = Stuff (b `xor` toRem)
  | otherwise            = error ("Set " ++ show b ++ " lacks stuff in " ++ show toRem)
stuffAddOnFloor f = stuffAdd . thingsOnFloor f
stuffRemOnFloor f = stuffRem . thingsOnFloor f

stuffClearFloor (Floor i) (Stuff b) = Stuff (b .&. mask)
  where mask = complement $ (bitCast allThings) `shiftL` (i * floorShift)
stuffAllOnFloor f = stuffNull . stuffClearFloor f
stuffAllOnTop = stuffAllOnFloor topFloor

stuffFromList :: [Things] -> Stuff
stuffFromList = foldr stuffAdd emptyStuff . zipWith thingsOnFloor allFloors

-- game rules

noGens = elemsNull . thingsGens
noZappables = elemsNull . zappables
zappables t = elemsSub (thingsChips t) (thingsGens t) 
thingsSafe t = noGens t || noZappables t

movableSets src = [twoThings t0 t1 | t0 <- ts, t1 <- ts, t0 <= t1]
  where ts = thingsToList src
safeMovableSets src dst = filter safe $ movableSets src
  where safe mov = thingsSafe (thingsRem mov src) && thingsSafe (thingsAdd mov dst)

performMove srcf dstf mov = stuffAddOnFloor dstf mov . stuffRemOnFloor srcf mov

performSafeMoves srcf dstf stuff = map perform movs
  where movs = safeMovableSets (stuffGet srcf stuff) (stuffGet dstf stuff)
        perform mov = performMove srcf dstf mov stuff

performSafeMovesFrom srcf stuff = do
  dstf <- adjFloors srcf
  stuff' <- performSafeMoves srcf dstf stuff
  return (dstf, stuff')

data World = World Floor Stuff
             deriving (Eq, Ord, Show)
worldStart = World groundFloor

instance Node World where
  adjacent (World srcf stuff) =
    map (uncurry World) $ performSafeMovesFrom srcf stuff

worldDone (World f stuff) = f == topFloor && stuffAllOnTop stuff
stepsToDone = pathLen . head . filter (worldDone . pathNode) . bfs

-- reading

readElem = fmap safeElem . aton
readThing (Syntax.Chip s) = chipThing <$> readElem s
readThing (Syntax.Gen s) = genThing <$> readElem s
readThings = fmap thingsFromList . mapM readThing
readStuff = evalSymTab . fmap stuffFromList . mapM readThings
readWorld = worldStart . readStuff
parse_prob = readWorld . Syntax.parse_prob
parse_file = fmap readWorld . Syntax.parse_file
