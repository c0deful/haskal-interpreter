module Runner where

import AbsHaskal
import Interpreter
import Memory
import Types
import Utils

-- EVALUATING, EXECUTING AND RUNNING PARSED SYNTAX TREE

evalInt :: Exp -> Env -> Interpreter Value
evalInt e env = do v <- eval e env
                   case v of
                       (VInt _)  -> return v
                       otherwise -> fail "Type mismatch: integer value expected"

evalBool :: Exp -> Env -> Interpreter Value
evalBool e env = do v <- eval e env
                    case v of
                       (VBool _) -> return v
                       otherwise -> fail "Type mismatch: boolean value expected"

eval :: Exp -> Env -> Interpreter Value
eval (EAdd e1 e2) env = do VInt v1 <- evalInt e1 env
                           VInt v2 <- evalInt e2 env
                           return $ VInt (v1 + v2)
eval (ESub e1 e2) env = do VInt v1 <- evalInt e1 env
                           VInt v2 <- evalInt e2 env
                           return $ VInt (v1 - v2)
eval (EMul e1 e2) env = do VInt v1 <- evalInt e1 env
                           VInt v2 <- evalInt e2 env
                           return $ VInt (v1 * v2)
eval (EDiv e1 e2) env = do VInt v1 <- evalInt e1 env
                           VInt v2 <- evalInt e2 env
                           if v2 == 0
                               then fail "Division by zero"
                               else return $ VInt (v1 `div` v2)
eval (EAnd e1 e2) env = do VBool v1 <- evalBool e1 env
                           VBool v2 <- evalBool e2 env
                           return $ VBool (v1 && v2)
eval (EOr e1 e2) env  = do VBool v1 <- evalBool e1 env
                           VBool v2 <- evalBool e2 env
                           return $ VBool (v1 || v2)
eval (EEq e1 e2) env  = do v1 <- eval e1 env
                           v2 <- eval e2 env
                           return $ VBool (v1 == v2)
eval (ENeq e1 e2) env = do v1 <- eval e1 env
                           v2 <- eval e2 env
                           return $ VBool (v1 /= v2)
eval (ELsr e1 e2) env = do v1 <- eval e1 env
                           v2 <- eval e2 env
                           return $ VBool (v1 < v2)
eval (EGtr e1 e2) env = do v1 <- eval e1 env
                           v2 <- eval e2 env
                           return $ VBool (v1 > v2)
eval (ELeq e1 e2) env = do v1 <- eval e1 env
                           v2 <- eval e2 env
                           return $ VBool (v1 <= v2)
eval (EGeq e1 e2) env = do v1 <- eval e1 env
                           v2 <- eval e2 env
                           return $ VBool (v1 >= v2)
eval (ENot e) env     = do VBool v <- evalBool e env
                           return $ VBool (not v)
eval (EBool e) env    = do return $ VBool (bool e)
eval (EInt e) env     = do return $ VInt e
eval (EVar e) env     = do (_, l) <- getVar e env
                           v <- getVal l
                           return v


exec :: Stmt -> Env -> Interpreter [String]
exec (SCompound []) env        = do return []
exec (SCompound (s:ss)) env    = do out1 <- exec s env
                                    out2 <- exec (SCompound ss) env
                                    return (out1 ++ out2)
exec (SWhile e s) env          = do VBool v <- evalBool e env
                                    if v 
                                        then exec (SCompound [s, SWhile e s]) env
                                        else return []
exec (SIf e s) env             = do VBool v <- evalBool e env
                                    if v
                                        then exec s env
                                        else return []
exec (SIfElse e s1 s2) env     = do VBool v <- evalBool e env
                                    if v
                                        then exec s1 env
                                        else exec s2 env
exec (SAssign id Assign e) env = do (t, l) <- getVar id env
                                    val <- case t of
                                	     TInt  -> evalInt e env
                                	     TBool -> evalBool e env
                                    storeVal l (Just val)
                                    return []
exec (SAssign id op e) env     = do val <- case op of
                                    	AssignAdd -> eval (EAdd (EVar id) e) env
                                    	AssignSub -> eval (ESub (EVar id) e) env
                                    	AssignMul -> eval (EMul (EVar id) e) env
                                    	AssignDiv -> eval (EDiv (EVar id) e) env
                                    	AssignAnd -> eval (EAnd (EVar id) e) env
                                    	AssignOr  -> eval (EOr (EVar id) e) env
                                    (_, l) <- getVar id env
                                    storeVal l (Just val)
                                    return []
exec (SIter id op) env         = do val <- case op of
                                        IterInc -> eval (EAdd (EVar id) (EInt 1)) env
                                        IterDec -> eval (ESub (EVar id) (EInt 1)) env
                                    (_, l) <- getVar id env
                                    storeVal l (Just val)
                                    return []
exec (SPrint e) env            = do v <- eval e env
                                    case v of
                                        (VInt i)  -> return [show i]
                                        (VBool b) -> return [show b]
exec (SProcCall id as) env     = do l <- getProcLoc id env
                                    Proc env' ps b <- getProc l
                                    env'' <- insertArgs as ps env env'
                                    run b env''


decl :: Decl -> Env -> Interpreter Env
decl (VarDecl t id) env     = do env' <- reserveVarLoc id t env
                                 (_, l) <- getVar id env'
                                 storeVal l Nothing
                                 return env'
decl (ProcDecl id ps b) env = do env' <- reserveProcLoc id env
                                 l <- getProcLoc id env'
                                 storeProc l (Proc env' ps b)
                                 return env'


run :: Block -> Env -> Interpreter [String]
run (ScopeBlock [] s) env     = do exec s env
run (ScopeBlock (d:ds) s) env = do env' <- decl d env
                                   run (ScopeBlock ds s) env'



-- BUILDING PROCEDURE ENVIRONMENT FROM ARGUMENTS

insertArgs :: [Arg] -> [Param] -> Env -> Env -> Interpreter Env
insertArgs (a:as) (p:ps) source target = do target' <- insertArgs as ps source target
                                            case p of
                                                ParamDecl PVar t id -> insertVarArg id a t source target'
                                                ParamDecl PVal t id -> insertValArg id a t source target'
insertArgs [] (p:_) source target      = do fail "Wrong number of arguments passed to procedure"
insertArgs (a:_) [] source target      = do fail "Wrong number of arguments passed to procedure"
insertArgs [] [] source target         = do return target

insertVarArg :: Ident -> Arg -> TypeSpec -> Env -> Env ->Interpreter Env
insertVarArg id (ExpArg e) t source target = case e of
                                                 (EVar v)  -> do (t', l) <- getVar v source
                                                                 if t' == t
                                                                     then assignVarLoc id t l target
                                                                     else fail "Wrong argument type"
                                                 otherwise -> do fail "Pass-by-name argument expected"

insertValArg :: Ident -> Arg -> TypeSpec -> Env -> Env ->Interpreter Env
insertValArg id (ExpArg e) t source target = do v <- eval e source
                                                case (v, t) of
                                                    (VInt _, TBool)  -> fail "Wrong argument type"
                                                    (VBool _, TInt)  -> fail "Wrong argument type"
                                                    otherwise        -> do target' <- reserveVarLoc id t target
                                                                           (_, l) <- getVar id target'
                                                                           storeVal l (Just v)
                                                                           return target'
