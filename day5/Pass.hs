module Pass where
import Data.Bits
import Data.Maybe
import qualified Data.ByteString as B
import qualified Crypto.Hash.MD5 as M

unstring :: String -> B.ByteString
unstring = B.pack . map (toEnum . fromEnum)

setup = M.update M.init . unstring

mthen b e = if b then e else Nothing

zerostart n bs = 
  loop n (B.unpack bs)
  where loop 0 (b:_) = Just (shiftR b 4)
        loop 1 (b:_) = mthen (b .&. 0xf0 == 0) $ Just (b .&. 0x0f)
        loop n (b:bs) = mthen (b == 0) $ loop (n - 2) bs

check n = zerostart n . M.finalize
generate c = map (M.update c . unstring . show) [0..]
decode n = catMaybes . map (check n) . generate . setup

solve = map (("0123456789abcdef" !!) . fromEnum) . take 8 . decode 5
