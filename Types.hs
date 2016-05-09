module Types where

import qualified Data.Map as M

import AbsHaskal

data Value = VInt Integer | VBool Bool deriving (Show, Ord, Eq)
data Proc = Proc Env [Param] Block deriving Show

newtype Loc = Loc Integer deriving (Show, Ord, Eq)

type Var    = (TypeSpec, Loc)
type VEnv   = M.Map Ident Var
type PEnv   = M.Map Ident Loc
type Env    = (VEnv, PEnv)

type Store  = (VStore, PStore)
type VStore = M.Map Loc (Maybe Value)
type PStore = M.Map Loc Proc