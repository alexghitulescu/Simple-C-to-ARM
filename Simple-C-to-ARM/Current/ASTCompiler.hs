
module ASTCompiler (
     comp,
     compE
) where

import Prelude hiding (EQ)
import Data.Foldable (toList)
import Data.Sequence
import Text.Parsec.Pos
import AST
import IAST
import Environment
import DAG
import VMInst

comp                            :: Prog -> IProg
comp p                          = fst $ runState' p

compE                           :: Prog -> IO IProg
compE p                         = case err of
                                        [] -> return prog
                                        xs -> do print xs
                                                 return (IPSeq [])
                                   where (prog, (_, _, err, _)) = runState' p
                                              
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

fresh                   :: ST Name
fresh                   =  S (\(n, env, e, c) -> ("@temp@" ++ show(n), (n+1, env, e, c)))

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

-- [names of arguments] -> where should it start
addFuncArgs             :: [Name] -> ST ()
addFuncArgs []          = return ()
addFuncArgs (n:ns)      = do addEnvVar n (P SB (0))
                             addFuncArgs ns

compProg                        :: Prog -> ST IProg
compProg (GlobalVar n pos)      = return (IGlobalVar n)
compProg (Fun n ns st)          = do addEnvLevel
                                     addFuncArgs ns
                                     stmt <- compStmt st
                                     remEnvLevel
                                     return (IFun n ns stmt)
compProg (PSeq [])              = return (IPSeq [])
compProg (PSeq xs)              = do list <- mapM compProg xs
                                     return (IPSeq list)
                
compStmt                        :: Stmt -> ST IStmt
compStmt (LocalVar n)           = do addEnvVar n (P SB 0)
                                     return (ILocalVar n)
compStmt (Assign n e)           = do posV <- getEnvVar n
                                     compAssign n e
compStmt (Print e)              = do let (seq, var) = transform e
                                     let stmt = toList $ seq |> (IPrint var)
                                     return (ISeqn stmt)
compStmt (Seqn [])              = return (ISeqn [])
compStmt (Seqn xs)              = do list <- mapM compStmt xs
                                     return (ISeqn (list))
compStmt (While e p)            = do let (seq, var) = transform e
                                     let stmt = toList seq 
                                     p' <- compStmt p
                                     return (IWhile stmt var p')
compStmt (If e p1 p2)           = do p1' <- compStmt p1
                                     p2' <- compStmt p2
                                     let (seq, var) = transform e
                                     let stmt = toList $ seq |> (IIf var p1' p2')
                                     return (ISeqn stmt)
compStmt (Ex e)                 = do let (seq, var) = transform e
                                     return (ISeqn $ toList seq)
compStmt (Return e)             = do let (seq, var) = transform e
                                     let stmt = toList $ seq |> (IReturn var)
                                     return (ISeqn stmt)

                                     
compAssign                      :: Name -> Expr -> ST IStmt
compAssign n (Val _ i)          = return (IAssign n (IVal i))
compAssign n (Var pos v)        = do posV <- getEnvVar2 v pos
                                     return (IAssign n (IVar v))
compAssign n expr               = do let (seq, var) = transform expr
                                     let stmt = toList $ seq |> (IAssign n var)
                                     return (ISeqn stmt)
                                     
              
jumpz                           :: Label -> Code
jumpz lb                        = [PUSH 0, CMPST, B EQ lb]
                                     