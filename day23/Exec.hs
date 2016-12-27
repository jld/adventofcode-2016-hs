module Exec where
import Insn
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

jmpRel s dpc = modifySTRef (statePC s) (+ dpc)

regRead s (Reg r) = VGM.read (stateRegs s) r
regWrite s (Reg r) = VGM.write (stateRegs s) r
regMod s (Reg r) f = VGM.modify (stateRegs s) f r

step prog s = do
  pc <- readSTRef $ statePC s
  insn <- VU.indexM prog pc
  applyInsn s insn

applyInsn s (Inc r) = do
  regMod s r (+ 1)
  jmpRel s 1

applyInsn s (Dec r) = do
  regMod s r (flip (-) 1)
  jmpRel s 1

applyInsn s (Tgl _) = error "applyInsn: toggle instruction in immutable program mode"

applyInsn s (Cpy src rd) = do
  val <- applySrc s src
  regWrite s rd val
  jmpRel s 1

applyInsn s (Jnz scond sdpc) = do
  cond <- applySrc s scond
  if cond /= 0 then do
    dpc <- applySrc s sdpc
    jmpRel s dpc
  else
    jmpRel s 1

applySrc _ (SrcImm (Imm i)) = return i
applySrc s (SrcReg r) = regRead s r
