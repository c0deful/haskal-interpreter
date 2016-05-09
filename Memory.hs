module Memory where

import qualified Data.Map as M

import AbsHaskal

import Interpreter
import Types
import Utils

-- ACCESS PROCEDURE ENVIRONMENT AND STORE

getProcLoc :: Ident -> Env -> Interpreter Loc
getProcLoc id (_, penv) = do case M.lookup id penv of
                                 Nothing -> fail "Nonexistent procedure"
                                 Just l  -> return l

reserveProcLoc :: Ident -> Env -> Interpreter Env
reserveProcLoc id (venv, penv) = Interpreter $ \(vs, ps) -> let l = alloc ps in
                                     Right ((venv, M.insert id l penv), (vs, ps))

getProc :: Loc -> Interpreter Proc
getProc l = Interpreter $ \(vs, ps) -> case M.lookup l ps of
                Nothing -> Left "Bad location"
                Just p  -> Right (p, (vs, ps))

storeProc :: Loc -> Proc -> Interpreter ()
storeProc l p = Interpreter $ \(vs, ps) -> Right ((), (vs, M.insert l p ps))



-- ACCESS VARIABLE ENVIRONMENT AND STORE

getVar :: Ident -> Env -> Interpreter Var
getVar id (venv, _) = do case M.lookup id venv of
                             Nothing -> fail "Unbound variable"
                             Just l  -> return l

reserveVarLoc :: Ident -> TypeSpec -> Env -> Interpreter Env
reserveVarLoc id t (venv, penv) = Interpreter $ \(vs, ps) -> let l = alloc vs in
                                     Right ((M.insert id (t, l) venv, penv), (vs, ps))

assignVarLoc :: Ident -> TypeSpec -> Loc -> Env -> Interpreter Env
assignVarLoc id t l (venv, penv) = Interpreter $ \(vs, ps) ->
                                     Right ((M.insert id (t, l) venv, penv), (vs, ps))

getVal :: Loc -> Interpreter Value
getVal l = Interpreter $ \(vs, ps) -> case M.lookup l vs of
               Nothing       -> Left "Bad location"
               Just Nothing  -> Left "Unassigned value"
               Just (Just v) -> Right (v, (vs, ps))

storeVal :: Loc -> Maybe Value -> Interpreter ()
storeVal l v = Interpreter $ \(vs, ps) -> Right ((), (M.insert l v vs, ps))