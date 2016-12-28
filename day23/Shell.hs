module Shell where
import Insn
import Exec
import Parse
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

day12 a0 prog =
  launch $ do
    regWrite (Reg 2) a0
    res <- whileOk $ step prog
    case res of
     Halt -> regRead (Reg 0)
     _ -> do
       pc <- getPC
       error $ "Bunny Fault: " ++ show res ++ " at PC " ++ show pc
