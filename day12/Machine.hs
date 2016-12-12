module Machine where
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector (Vector, (!), (!?), (//))

type Val = Integer
data Src = FromReg Reg
         | FromImm Val
         deriving (Eq, Show)

data Insn = ICpy Src Reg
          | IInc Reg
          | IDec Reg
          | IJnz Src Int
          deriving (Eq, Show)

newtype Reg = Reg Int
            deriving (Eq, Show)
type RegFile = Vector Val
num_regs = 4

regfile :: RegFile
regfile = V.replicate num_regs 0

reg_get :: Reg -> RegFile -> Val
reg_get (Reg i) = (! i)

reg_set :: Reg -> Val -> RegFile -> RegFile
reg_set (Reg i) v = (// [(i, v)])

reg_mod :: Reg -> (Val -> Val) -> RegFile -> RegFile
reg_mod r f rf = reg_set r (f $ reg_get r rf) rf

eval_src (FromReg r) = reg_get r
eval_src (FromImm v) = const v

type Next = Maybe Cfg
data Cfg = CCpy Src Reg Next
         | CInc Reg Next
         | CDec Reg Next
         | CJnz Src Next Next

xlate1 f i (ICpy src reg) = CCpy src reg (f $ succ i)
xlate1 f i (IInc reg) = CInc reg (f $ succ i)
xlate1 f i (IDec reg) = CDec reg (f $ succ i)
xlate1 f i (IJnz src ifnz) = CJnz src (f $ i + ifnz) (f $ succ i)

tangle :: [Insn] -> Next
tangle is = iv !? 0
  where iv = V.fromList $ zipWith (xlate1 (iv !?)) [0..] is

step :: Cfg -> RegFile -> (Next, RegFile)
step (CCpy src dst next) rf = (next, reg_set dst (eval_src src rf) rf)
step (CInc reg next) rf = (next, reg_mod reg (+ 1) rf)
step (CDec reg next) rf = (next, reg_mod reg (flip (-) 1) rf)
step (CJnz src ifnz ifz) rf = (next, rf)
  where next = if eval_src src rf == 0 then ifz else ifnz

run :: Next -> RegFile
run start = loop (start, regfile)
  where loop (Nothing, rf) = rf
        loop (Just cfg, rf) = loop $ step cfg rf

irun = run . tangle
