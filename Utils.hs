module Utils where

import qualified Data.Map as M

import AbsHaskal

import Interpreter
import Types


emptyEnv :: Env
emptyEnv = (M.empty, M.empty)

emptyStore :: Store
emptyStore = (M.empty, M.empty)

bool :: Boolean -> Bool
bool b = case b of
             BoolT -> True
             BoolF -> False

alloc :: M.Map Loc a -> Loc
alloc s = if (M.null s)
	          then (Loc 1)
	          else (Loc (l + 1)) where (Loc l, _) = M.findMax s