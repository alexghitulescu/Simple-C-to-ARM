
module ASTCompiler (
     comp,
     compE
) where

import Prelude hiding (EQ)
import AST
import Environment
import VMInst

comp                            :: Prog -> Code
comp p                          = fst $ runState' p

compE                           :: Prog -> IO Code
compE p                         = case err of
                                        [] -> do print "no error"
                                                 return code
                                        xs -> do print xs
                                                 return []
                                   where (code, (_, _, err)) = runState' p
                                              
runState' p = runState (compProg p) (0, emptyTop, [])                                              

type Error = [String]

-- State Monad 
-- ===========

-- Declaration for the state monad and a new type runState to save writing State -(a, State).

data ST a     = S { runState :: State -> (a, State) }
type State    = (Integer, Env Imd, Error)

apply         :: ST a -> State -> (a,State)
apply (S f)  = f 

instance Monad ST where
      -- return :: a -> ST a
      return x   = S (\s -> (x,s))

      -- (>>=)  :: ST a -> (a -> ST b) -> ST b
      st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')

      
-- The function that generates the fresh labels. It is of type ST showing that it has a hidden state. 

fresh                   :: ST Label
fresh                   =  S (\(n, env, e) -> (V n, (n+1, env, e)))

writeError              :: Error -> ST ()
writeError e            = S ((\(n, env, _) -> ((), (n+1, env, e))))

addEnvLevel             :: ST ()
addEnvLevel             =  S (\(n, env, e) -> ((), (n, addLevel env, e)))

remEnvLevel             :: ST ()
remEnvLevel             =  S (\(n, env, e) -> ((), (n, removeLevel env, e)))

addEnvVar               :: Name -> Imd -> ST ()
addEnvVar q (P r d)     =  S (\(n, env, e) -> ((), (n, env `addVar` (q,P r d), e)))

getEnvVar               :: Name -> ST Imd
getEnvVar q             =  S (\(n, env, e) -> case env `getVar` q of 
                                                Nothing -> (P SB 0, (n, env, ("could not find " ++ (show q)):e))
                                                Just p  -> (p, (n, env, e)))
                       
setEnvDisplacement      :: Integer -> ST ()
setEnvDisplacement i    =  S (\(n, env, e) -> ((), (n, env `setDisplacement` i, e)))

getEnvDisplacement      :: ST Integer
getEnvDisplacement      =  S (\(n, env, e) -> (displacement env, (n, env, e)))

addEnvDisplacement      :: Integer -> ST ()
addEnvDisplacement i    =  S (\(n, env, e) -> ((), (n, env `addDisplacement` i, e)))


-- [names of arguments] -> where should in start
addFuncArgs             :: [Name] -> Integer -> ST ()
addFuncArgs [] _        = return ()
addFuncArgs (n:ns) i    = do addEnvVar n (P SB (-i))
                             addFuncArgs ns (i + 1)

compProg                        :: Prog -> ST Code
compProg (GlobalVar n)          = do addEnvVar n (P (G n) 0)
                                     return [ADDRESS n]
compProg (Fun n ns st)          = do addEnvLevel
                                     --take space for arguments on the stack
                                     addFuncArgs ns 1
                                     setEnvDisplacement 2
                                     code <- (compStmt st)
                                     envDis <- getEnvDisplacement
                                     remEnvLevel
                                     --save SB, LR and execute the code and increment SP by the number of args.
                                     return ([LABEL (N n), PUSHV SB, MOV SB (P SP 0), PUSHV LR] ++ code ++ 
                                                [SUB SP SP (VAL (envDis - 2)), POP LR, POP SB, SUB SP SP (VAL (toInteger(length ns)))
                                                ,PUSHV (R "r0"), BX NONE LR]) 
                                                -- restore SP to before arguments
compProg (PSeq [    ])          = return []
compProg (PSeq (x:xs))          = do code <- compProg x
                                     code' <- compProg (PSeq xs)
                                     return (code ++ code')
                
compStmt                        :: Stmt -> ST Code
compStmt (LocalVar n)           = do dis <- getEnvDisplacement
                                     addEnvVar n (P SB dis)
                                     addEnvDisplacement 1
                                     return [ADD SP SP (VAL 1)]
compStmt (Assign v e)           = do posV <- getEnvVar v
                                     expr <- compExpr e
                                     return (expr ++ [POP (R "r5"), STR (R "r5") posV])
compStmt (Print e)              = do expr <- compExpr e
                                     return (expr ++ [PRINT])
compStmt (Seqn [    ])          = return []
compStmt (Seqn (x:xs))          = do code <- compStmt x
                                     code' <- compStmt (Seqn xs)
                                     return (code ++ code')
compStmt (While e p)            = do lb <- fresh
                                     code <- compStmt p
                                     lb' <- fresh
                                     expr <- compExpr e
                                     return ([LABEL lb] ++ expr ++ (jumpz lb') ++ code ++ [B NONE lb, LABEL lb'])
compStmt (If e p1 (Seqn []))    = do lb <- fresh
                                     code <- compStmt p1
                                     expr <- compExpr e
                                     return (expr ++ (jumpz lb) ++ code ++ [LABEL lb])
compStmt (If e p1 p2)           = do lb <- fresh
                                     lb' <- fresh
                                     code <- compStmt p1
                                     code' <- compStmt p2
                                     expr <- compExpr e
                                     return (expr ++ (jumpz lb) ++ code ++ [B NONE lb', LABEL lb] ++ code' ++ [LABEL lb'])
compStmt (Ex e)                 = do expr <- compExpr e
                                     return (expr ++ [POP (R "r12")])
compStmt (Return e)             = do expr <- compExpr e
                                     return (expr ++ [POP (R "r0")])
              

jumpz                           :: Label -> Code
jumpz lb                        = [PUSH 0, CMPST, B EQ lb]
                                     
compExprs                     :: [Expr] -> ST Code
compExprs []                  = return []
compExprs (e:es)              = do expr <- compExpr e
                                   expr' <- compExprs es
                                   return (expr ++ expr')
                                      
compExpr                      :: Expr -> ST Code
compExpr (Val n)              = return [PUSH n]
compExpr (Var v)              = do posV <- getEnvVar v
                                   return [LDR (R "r5") posV, PUSHV (R "r5")]
compExpr (App op e1 e2)       = do expr1 <- compExpr e1
                                   expr2 <- compExpr e2
                                   return (expr1 ++ expr2 ++ [DO op])
compExpr (Apply n e)          = do exprs <- compExprs e
                                   return (exprs ++ [BL NONE (N n)])
                                  