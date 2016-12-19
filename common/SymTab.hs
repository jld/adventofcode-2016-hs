module SymTab where
import qualified Data.Map.Strict as M
import Control.Monad.State.Lazy

newtype SymTab a = SymTab (M.Map a Int)
                   deriving Show
newTab = SymTab M.empty

aton' :: Ord a => a -> (SymTab a) -> (Int, SymTab a)
aton' sym st@(SymTab tab) = case M.lookup sym tab of
  Just n -> (n, st)
  Nothing -> (n, SymTab tab')
    where n = M.size tab
          tab' = M.insert sym n tab

aton :: Ord a => a -> State (SymTab a) Int
aton = state . aton'

-- TODO if I ever need it: add the reverse map and ntoa

runSymTab = flip runState newTab
evalSymTab = flip evalState newTab
