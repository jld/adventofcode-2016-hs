module Exec where
import Insn
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.STRef
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

kNumRegs = 4

data State s = State { statePC :: STRef s Int,
                       stateRegs :: VU.MVector s Int }

newState = do
  pc <- newSTRef 0
  regs <- VGM.replicate kNumRegs 0
  return $ State pc regs

-- I feel slightly dirty using the constructor directly like this.
wranglePC = ReaderT . (. statePC)
wrangleRegs = ReaderT . (. stateRegs)

getPC = wranglePC readSTRef
jmpRel dpc = wranglePC $ flip modifySTRef (+ dpc)

regRead (Reg r) = wrangleRegs (\regs -> VGM.read regs r)
regWrite (Reg r) v = wrangleRegs (\regs -> VGM.write regs r v)
regMod (Reg r) f = wrangleRegs (\regs -> VGM.modify regs f r)

step prog = do
  pc <- getPC
  insn <- VU.indexM prog pc
  applyInsn insn

applyInsn (Inc r) = do
  regMod r (+ 1)
  jmpRel 1

applyInsn (Dec r) = do
  regMod r (flip (-) 1)
  jmpRel 1

applyInsn (Tgl _) = error "applyInsn: toggle instruction in immutable program mode"

applyInsn (Cpy src rd) = do
  val <- applySrc src
  regWrite rd val
  jmpRel 1

applyInsn (Jnz scond sdpc) = do
  cond <- applySrc scond
  if cond /= 0 then do
    dpc <- applySrc sdpc
    jmpRel dpc
  else
    jmpRel 1

applySrc (SrcImm (Imm i)) = return i
applySrc (SrcReg r) = regRead r
