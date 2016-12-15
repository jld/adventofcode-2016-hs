module YoloCrypto where
-- FIXME: again with the copypasting (from day 5)
import Data.Bits
import Data.List
import qualified Data.ByteString as B
import qualified Crypto.Hash.MD5 as M

unstring :: String -> B.ByteString
unstring = B.pack . map (toEnum . fromEnum)

nybbles [] = []
nybbles (b:bs) = (b `shiftR` 4):(b .&. 0x0f):(nybbles bs)

setup = M.update M.init . unstring
generate' c = map (M.update c . unstring . show) [0..]
generate = map (nybbles . B.unpack . M.finalize) . generate'

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

solve' = keys . generate . setup
solve = (!! 63) . solve'
