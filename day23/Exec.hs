{-# LANGUAGE RankNTypes #-}
module Exec where
import Insn
import Control.Monad.ST
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

data Snapshot = Snapshot { snapPC :: Int,
                           snapRegs :: VU.Vector Int }
                deriving (Eq, Ord, Show)

snapshot = do
  State mpc mregs <- ask
  pc <- lift $ readSTRef mpc
  regs <- lift $ VG.freeze mregs
  return $ Snapshot pc regs

data StepResult = Ok
                | Halt
                | BusError
                deriving (Eq, Show)

newState = do
  pc <- newSTRef 0
  regs <- VGM.replicate kNumRegs 0
  return $ State pc regs

launch :: (forall s. ReaderT (State s) (ST s) a) -> a
launch thing = runST $ newState >>= runReaderT thing

whileOk mx = do
  res <- mx
  case res of
   Ok -> whileOk mx
   _  -> return res

-- I feel slightly dirty using the constructor directly like this.
wranglePC = ReaderT . (. statePC)
wrangleRegs = ReaderT . (. stateRegs)

getPC = wranglePC readSTRef
jmpRel dpc = wranglePC (flip modifySTRef (+ dpc)) >> return Ok

getPCGuarded bound = check <$> getPC
  where check pc
          | pc >= 0 && pc < bound = Just pc
          | otherwise             = Nothing

regRead (Reg r) = wrangleRegs (\regs -> VGM.read regs r)
regWrite (Reg r) v = wrangleRegs (\regs -> VGM.write regs r v)
regMod (Reg r) f = wrangleRegs (\regs -> VGM.modify regs f r)

step prog = do
  maybePC <- getPCGuarded $ VU.length prog
  case maybePC of
   Just pc -> do
     insn <- VU.indexM prog pc
     applyInsn insn
   Nothing ->
     return Halt

stepMut prog = do
  maybePC <- getPCGuarded $ VUM.length prog
  case maybePC of
   Just pc -> do
     insn <- VUM.read prog pc
     applyInsnMut prog insn
   Nothing ->
     return Halt

applyInsnMut prog (Tgl srel) = do
  pc <- getPC
  rel <- applySrc srel
  lift $ toggleInsn prog (pc + rel)
  jmpRel 1

applyInsnMut _ insn =
  applyInsn insn

applyInsn Nop =
  jmpRel 1

applyInsn (Inc r) = do
  regMod r (+ 1)
  jmpRel 1

applyInsn (Dec r) = do
  regMod r (flip (-) 1)
  jmpRel 1

applyInsn (Tgl _) =
  return BusError

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
