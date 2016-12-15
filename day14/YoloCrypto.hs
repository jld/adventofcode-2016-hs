module YoloCrypto where
-- FIXME: again with the copypasting (from day 5)
import Data.Bits
import Data.Char
import Data.List
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Crypto.Hash.MD5 as M

unstring :: String -> B.ByteString
unstring = B.pack . map (toEnum . fromEnum)

nybbles [] = []
nybbles (b:bs) = (b `shiftR` 4):(b .&. 0x0f):(nybbles bs)

tohex :: Word8 -> Char
tohex = tohex' . fromEnum
tohex' n
  | n < 10 = chr (ord '0' + n)
  | n < 16 = chr (ord 'a' + (n - 10))
  | otherwise = error ("tohex: " ++ show n ++ "out of range")

hash1 = nybbles . B.unpack . M.hash . unstring
hexhash = hash1 . map tohex
hash2016 = foldr (.) hash1 (replicate 2016 hexhash)

keyify :: String -> Integer -> String
keyify k i = k ++ show i
generate h k = map (h . keyify k) [0..]

triples (a:bs@(b:c:_))
  | (a == b && b == c) = a:(triples bs)
  | otherwise          = triples bs
triples _ = []

has_five x = isInfixOf (replicate 5 x)

keys stream = do
  (index, (ns:nss)) <- zip [0..] $ tails stream
  trip <- take 1 $ triples ns
  maybe_five <- take 1 $ filter (has_five trip) $ take 1000 nss
  return index

solve' h = keys . generate h
solve h = (!! 63) . solve' h
