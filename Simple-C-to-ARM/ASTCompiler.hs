
module ASTCompiler (
     comp,
     compE
) where

import Prelude hiding (EQ)
import Data.Foldable (toList)
import Data.Sequence
import Text.Parsec.Pos
import AST
import Environment
import VMInst

comp                            :: Prog -> Code
comp p                          = toList code
                                        where (_, (_, _, err, code)) = runState' p

compE                           :: Prog -> IO Code
compE p                         = case err of
                                        [] -> do print "no error"
                                                 return $ toList code
                                        xs -> do print xs
                                                 return []
                                   where (_, (_, _, err, code)) = runState' p
                                              
runState' p = runState (compProg p) (0, emptyTop, [], empty)                                              

type Error = [String]

-- State Monad 
-- ===========

-- Declaration for the state monad and a new type runState to save writing State -(a, State).

data ST a     = S { runState :: State -> (a, State) }
type State    = (Integer, Env Imd, Error, Seq Inst)

apply         :: ST a -> State -> (a,State)
apply (S f)  = f 

instance Monad ST where
      -- return :: a -> ST a
      return x   = S (\s -> (x,s))

      -- (>>=)  :: ST a -> (a -> ST b) -> ST b
      st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')

      
-- The function that generates the fresh labels. It is of type ST showing that it has a hidden state. 

fresh                   :: ST Label
fresh                   =  S (\(n, env, e, c) -> (V n, (n+1, env, e, c)))

emit                    :: Inst -> ST ()
emit i                  = S (\(n, env, e, c) -> ((), (n+1, env, e, c |> i)))

emitCode                :: Code -> ST ()
emitCode c'             = S (\(n, env, e, c) -> ((), (n+1, env, e, c >< fromList c')))

writeError              :: String -> ST ()
writeError e            =  S ((\(n, env, err, c) -> ((), (n, env, e:err, c))))

addEnvLevel             :: ST ()
addEnvLevel             =  S (\(n, env, e, c) -> ((), (n, addLevel env, e, c)))

remEnvLevel             :: ST ()
remEnvLevel             =  S (\(n, env, e, c) -> ((), (n, removeLevel env, e, c)))

addEnvVar               :: Name -> Imd -> ST ()
addEnvVar q (P r d)     =  S (\(n, env, e, c) -> ((), (n, env `addVar` (q,P r d), e, c)))

getEnvVar               :: Name -> ST Imd
getEnvVar q             =  S (\(n, env, e, c) -> case env `getVar` q of 
                                                Nothing -> (P SB 0, (n, env, ("could not find " ++ (show q)):e, c))
                                                Just p  -> (p, (n, env, e, c)))

getEnvVar2               :: Name -> SourcePos -> ST Imd
getEnvVar2 q p           = do env <- getEnv 
                              case env `getVar` q of 
                                                Nothing -> do let l = sourceLine p
                                                              let c = sourceColumn p
                                                              writeError ("could not find " ++ (show q) ++ "line: " ++ show l ++ ", column: " ++ show c)
                                                              return (P SB 0)
                                                Just p  -> return p
                                                
setEnvDisplacement      :: Integer -> ST ()
setEnvDisplacement i    =  S (\(n, env, e, c) -> ((), (n, env `setDisplacement` i, e, c)))

getEnvDisplacement      :: ST Integer
getEnvDisplacement      =  S (\(n, env, e, c) -> (displacement env, (n, env, e, c)))

addEnvDisplacement      :: Integer -> ST ()
addEnvDisplacement i    =  S (\(n, env, e, c) -> ((), (n, env `addDisplacement` i, e, c)))

getEnv                  :: ST (Env Imd)
getEnv                  =  S (\(n, env, e, c) -> (env, (n, env, e, c)))


-- [names of arguments] -> where should in start
addFuncArgs             :: [Name] -> Integer -> ST ()
addFuncArgs [] _        = return ()
addFuncArgs (n:ns) i    = do addEnvVar n (P SB (-i))
                             addFuncArgs ns (i + 1)

compProg                        :: Prog -> ST ()
compProg (GlobalVar n pos)      = do addEnvVar n (P (G n) 0)
                                     emit       (ADDRESS n)
compProg (Fun n ns st )         = do addEnvLevel
                                     --take space for arguments on the stack
                                     addFuncArgs ns 1
                                     setEnvDisplacement 2
                                     envDis <- getEnvDisplacement
                                     --save SB, LR and execute the code and increment SP by the number of args.
                                     emitCode   [LABEL (N n), PUSHV SB, MOV SB (P SP 0), PUSHV LR]
                                     compStmt st
                                     emitCode   [SUB SP SP (VAL (envDis - 2)), POP LR, POP SB]
                                     emit       (SUB SP SP (VAL (toInteger(Prelude.length ns))))
                                     emitCode   [PUSHV (R "r0"), BX NONE LR]
                                     -- restore SP to before arguments
                                     remEnvLevel
compProg (PSeq [    ])          = return ()
compProg (PSeq (x:xs))          = do compProg x
                                     compProg (PSeq xs)
                
compStmt                        :: Stmt -> ST ()
compStmt (LocalVar n)           = do dis <- getEnvDisplacement
                                     addEnvVar n (P SB dis)
                                     addEnvDisplacement 1
                                     emit       (ADD SP SP (VAL 1))
compStmt (Assign v e)           = do posV <- getEnvVar v
                                     compExpr e
                                     emitCode   [POP (R "r5"), STR (R "r5") posV]
compStmt (Print e)              = do compExpr e
                                     emit       PRINT
compStmt (Seqn [    ])          = return ()
compStmt (Seqn (x:xs))          = do compStmt x
                                     compStmt (Seqn xs)
compStmt (While e p)            = do lb <- fresh
                                     lb' <- fresh
                                     emit (LABEL lb) 
                                     compExpr e 
                                     emitCode   (jumpz lb')
                                     compStmt p
                                     emitCode   [B NONE lb, LABEL lb']
compStmt (If e p1 (Seqn []))    = do lb <- fresh
                                     compExpr e
                                     emitCode (jumpz lb)
                                     compStmt p1
                                     emit (LABEL lb)
compStmt (If e p1 p2)           = do lb <- fresh
                                     lb' <- fresh
                                     compExpr e
                                     emitCode (jumpz lb) 
                                     compStmt p1 
                                     emitCode [B NONE lb', LABEL lb] 
                                     compStmt p2 
                                     emit (LABEL lb')
compStmt (Ex e)                 = do compExpr e
                                     emit (SUB SP SP (VAL 1))
compStmt (Return e)             = do compExpr e
                                     emit (POP (R "r0"))

              
jumpz                           :: Label -> Code
jumpz lb                        = [PUSH 0, CMPST, B EQ lb]
                                     
{-transformExpr                           :: Expr -> ST [Expr]
transformExpr (Val n)                   = return [Val n]
transformExpr (Var v)                   = return [Var v]
transformExpr (App op (Val m) (Val n))  = return [Val (compOp m n op)]
transformExpr (App op (Val m) (Var n))  = return [App op (Val m) (Var n)]
transformExpr (App op (Var m) (Val n))  = return [App op (Var m) (Val n)]
transformExpr (App op (Var m) (Var n))  = return [App op (Var m) (Var n)]-}

                                     
compExprs                     :: [Expr] -> ST ()
compExprs []                  = return ()
compExprs (e:es)              = do compExpr e
                                   compExprs es
                                      
compExpr                      :: Expr -> ST ()
compExpr (Val pos n)          = emit (PUSH n)
compExpr (Var pos v)          = do posV <- getEnvVar2 v pos
                                   emit (LDR (R "r5") posV)
                                   emit (PUSHV (R "r5"))
compExpr (App pos op e1 e2)   = do compExpr e1
                                   compExpr e2
                                   emit (DO op)
compExpr (Apply n e)          = do compExprs e
                                   emit (BL NONE (N n))

compOp          :: Integer -> Integer -> Op -> Integer
compOp m n Add  = m + n
compOp m n Sub  = m - n
compOp m n Mul  = m * n
compOp m n Div  = m `div` n
                                     