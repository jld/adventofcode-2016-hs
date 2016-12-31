module Beep where
import Insn
import Exec
import Parse
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.Strict
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.STRef

type BeepM s = RWST Prog [Int] History (ExecM s)
type History = M.Map Snapshot Int
newHistory = M.empty :: History

theProg :: BeepM s Prog
theProg = ask

stateLookup :: Snapshot -> BeepM s (Maybe Int)
stateLookup snap = M.lookup snap <$> get

stateAdd :: Snapshot -> BeepM s ()
stateAdd snap = modify (\m -> M.insert snap (M.size m) m)

handleOut :: StepResult -> BeepM s (Maybe Int)
handleOut (Output o) = do
  snap <- lift snapshot
  maybeFound <- stateLookup snap
  when (isNothing maybeFound) $ do
    stateAdd snap
    tell [o]
  return maybeFound

handleOut Ok =
  return Nothing

handleOut failure = do
  pc <- lift getPC
  -- TODO, maybe: deal with halting programs.
  error ("Bunny fault: " ++ show failure ++ " at PC " ++ show pc)

stepOut :: BeepM s (Maybe Int)
stepOut = handleOut =<< (lift . whileOk . step) =<< theProg

whileNothing mx = mx >>= doneYet
  where doneYet Nothing = whileNothing mx
        doneYet (Just x) = return x

runOut :: BeepM s Int
runOut = whileNothing stepOut

experiment prog a0 = launch $ do
  regWrite (Reg 0) a0
  evalRWST runOut prog newHistory

isClock ostinato (backJump, stuff) = prefixCheck && periodCheck
  where prefixCheck = all id $ zipWith (==) stuff $ cycle ostinato
        periodCheck = period `mod` length ostinato == 0
        period = length stuff - backJump

findClock ost prog = head $ filter (isClock ost . experiment prog) [0..]
