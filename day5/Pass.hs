module Pass where
import Data.Bits
import Data.Maybe
import qualified Data.ByteString as B
import qualified Crypto.Hash.MD5 as M

unstring :: String -> B.ByteString
unstring = B.pack . map (toEnum . fromEnum)

setup = M.update M.init . unstring

nybbles [] = []
nybbles (b:bs) = (b `shiftR` 4):(b .&. 0x0f):(nybbles bs)

zerotrim 0 xs = Just xs
zerotrim n (0:xs) = zerotrim (n - 1) xs
zerotrim _ _ = Nothing

check n = zerotrim n . nybbles . B.unpack . M.finalize
generate c = map (M.update c . unstring . show) [0..]
decode n = catMaybes . map (check n) . generate . setup
decode1 n = map head . decode n

tohex = ("0123456789abcdef" !!) . fromEnum
solve = map tohex . take 8 . decode1 5

demux1 x0 = map tail . filter (\xs -> head xs == x0)
demux stream = map (flip demux1 stream) [0..]

decode2 n = map (head . head) . demux . decode n
solve2 = map tohex . take 8 . decode2 5
