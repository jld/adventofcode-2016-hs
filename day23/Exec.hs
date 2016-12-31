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

type ExecM s = ReaderT (State s) (ST s)

data State s = State { statePC :: STRef s Int,
                       stateRegs :: VU.MVector s Int }

data Snapshot = Snapshot { snapPC :: Int,
                           snapRegs :: VU.Vector Int }
                deriving (Eq, Ord, Show)

snapshot :: ExecM s Snapshot
snapshot = do
  State mpc mregs <- ask
  pc <- lift $ readSTRef mpc
  regs <- lift $ VG.freeze mregs
  return $ Snapshot pc regs

type ExecRes s = ExecM s StepResult
data StepResult = Ok
                | Halt
                | BusError
                | Output Int
                deriving (Eq, Show)

newState = do
  pc <- newSTRef 0
  regs <- VGM.replicate kNumRegs 0
  return $ State pc regs

launch :: (forall s. ExecM s a) -> a
launch thing = runST $ newState >>= runReaderT thing

whileOk mx = do
  res <- mx
  case res of
   Ok -> whileOk mx
   _  -> return res

-- I feel slightly dirty using the constructor directly like this.
wranglePC = ReaderT . (. statePC)
wrangleRegs = ReaderT . (. stateRegs)

getPC :: ExecM s Int
getPC = wranglePC readSTRef

jmpRel :: Int -> ExecRes s
jmpRel dpc = wranglePC (flip modifySTRef (+ dpc)) >> return Ok

checkPC progSize allegedPC
  | allegedPC >= 0
  , allegedPC < progSize = Just allegedPC
  | otherwise            = Nothing

getPCGuarded progSize = checkPC progSize <$> getPC

regRead :: Reg -> ExecM s Int
regRead (Reg r) = wrangleRegs (\regs -> VGM.read regs r)

regWrite :: Reg -> Int -> ExecM s ()
regWrite (Reg r) v = wrangleRegs (\regs -> VGM.write regs r v)

regMod :: Reg -> (Int -> Int) -> ExecM s ()
regMod (Reg r) f = wrangleRegs (\regs -> VGM.modify regs f r)

step :: Prog -> ExecRes s
step prog = do
  maybePC <- getPCGuarded $ VU.length prog
  case maybePC of
   Just pc -> do
     insn <- VU.indexM prog pc
     applyInsn insn
   Nothing ->
     return Halt

stepMut :: MProg s -> ExecRes s
stepMut prog = do
  maybePC <- getPCGuarded $ VUM.length prog
  case maybePC of
   Just pc -> do
     insn <- VUM.read prog pc
     applyInsnMut prog insn
   Nothing ->
     return Halt


applyInsnMut :: MProg s -> Insn -> ExecRes s

applyInsnMut prog (Tgl srel) = do
  pc <- getPC
  rel <- applySrc srel
  case checkPC (VUM.length prog) (pc + rel) of
   Just dst -> lift $ toggleInsn prog dst
   Nothing  -> return ()
  jmpRel 1

applyInsnMut _ insn =
  applyInsn insn


applyInsn :: Insn -> ExecRes s

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

applyInsn (Out src) = do
  val <- applySrc src
  Ok <- jmpRel 1
  return $ Output val

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

applySrc :: Src -> ExecM s Int
applySrc (SrcImm (Imm i)) = return i
applySrc (SrcReg r) = regRead r
