module Beep where
import Insn
import Exec
import Parse
import Control.Monad
import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.STRef

emit :: Monad m => a -> WriterT [a] m ()
emit = tell . pure

-- This was going to be a starting point, maybe, but it doesn't work
-- because of strictness.
{-
filterOut (Output o) = emit o >> return Ok
filterOut other      = return other

stepOut prog = lift (step prog) >>= filterOut

experiment a0 prog =
  launch $ do
    regWrite (Reg 0) a0
    execWriterT $ whileOk $ stepOut prog
-}

newtype StateLog s = StateLog (STRef s (M.Map Snapshot Int))
newLog = StateLog <$> newSTRef M.empty

stateLookup (StateLog lr) snap = M.lookup snap <$> readSTRef lr 
stateAdd (StateLog lr) snap = modifySTRef lr (\m -> M.insert snap (M.size m) m)

handleOut _   Ok =
  return Nothing

handleOut log (Output o) = do
  snap <- lift snapshot
  maybeFound <- lift $ lift $ stateLookup log snap
  when (isNothing maybeFound) $ do
    lift $ lift $ stateAdd log snap
    emit o
  return maybeFound

handleOut _   other = do
  pc <- lift getPC
  error ("Bunny fault " ++ show other ++ " at PC " ++ show pc)

-- Another ReaderT for these args I keep threading around might be
-- nice, but the lift stacks are getting irritating as it is.  Maybe
-- there's a way to factor them back into elegance, but, meh.
stepOut prog log = (lift $ whileOk $ step prog) >>= handleOut log

whileNothing mx = mx >>= f
  where f Nothing = whileNothing mx
        f (Just x) = return x

runOut prog = (lift $ lift $ newLog) >>= (whileNothing . stepOut prog)

experiment prog a0 = launch $ do
  regWrite (Reg 0) a0
  runWriterT $ runOut prog

isClock ostinato (backJump, stuff) = prefixCheck && periodCheck
  where prefixCheck = all id $ zipWith (==) stuff $ cycle ostinato
        periodCheck = period `mod` length ostinato == 0
        period = length stuff - backJump

findClock ost prog = head $ filter (isClock ost . experiment prog) [0..]
