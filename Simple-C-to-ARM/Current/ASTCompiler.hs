
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
                                   where (prog, (_, err)) = runState' p
                                              
runState' p = runState (compProg p) (emptyTop, [])                                              

type Error = [String]

-- State Monad 
-- ===========

-- Declaration for the state monad and a new type runState to save writing State -(a, State).

data ST a     = S { runState :: State -> (a, State) }
type State    = (Env Imd, Error)

apply         :: ST a -> State -> (a,State)
apply (S f)  = f 

instance Monad ST where
      -- return :: a -> ST a
      return x   = S (\s -> (x,s))

      -- (>>=)  :: ST a -> (a -> ST b) -> ST b
      st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')

      

writeError              :: String -> ST ()
writeError e            =  S ((\(env, err) -> ((), (env, e:err))))

addEnvLevel             :: ST ()
addEnvLevel             =  S (\(env, e) -> ((), (addLevel env, e)))

remEnvLevel             :: ST ()
remEnvLevel             =  S (\(env, e) -> ((), (removeLevel env, e)))

addEnvVar               :: Name -> Imd -> ST ()
addEnvVar q (P r d)     =  S (\(env, e) -> ((), (env `addVar` (q,P r d), e)))

getEnvVar                :: Name -> SourcePos -> ST Imd
getEnvVar q p            = do env <- getEnv 
                              case env `getVar` q of 
                                                Nothing -> do writeError $ "undefined variable " ++ show q ++ " near: " ++ show p
                                                              return (P SB 0)
                                                Just p  -> return p

getEnv                  :: ST (Env Imd)
getEnv                  =  S (\(env, e) -> (env, (env, e)))

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
compStmt (Assign pos n e)       = do posV <- getEnvVar n pos
                                     compAssign n e
compStmt (Print e)              = tryExpr [Int] e $ do { let (seq, var) = transform e
                                                       ; let stmt = toList $ seq |> (IPrint var)
                                                       ; return (ISeqn stmt)
                                                       }
compStmt (Seqn  [])             = return (ISeqn [])
compStmt (SeqnE [])             = return (ISeqn [])
compStmt (Seqn  xs)             = do list <- mapM compStmt xs
                                     return (ISeqn list)
compStmt (SeqnE xs)             = do list <- mapM compStmt xs
                                     return (ISeqnE list)
compStmt (While e p)            = tryExpr intAndBool e $ do { let (seq, var) = transform e
                                                            ; let stmt = toList seq 
                                                            ; p' <- compStmt p
                                                            ; return (IWhile stmt var p')
                                                            }
compStmt (If e p1 p2)           = tryExpr intAndBool e $ do { p1' <- compStmt p1
                                                            ; p2' <- compStmt p2
                                                            ; let (seq, var) = transform e
                                                            ; let stmt = toList $ seq |> (IIf var p1' p2')
                                                            ; return (ISeqn stmt)
                                                            }
compStmt (Ex e)                 = tryExpr anyType e $ do { let (seq, var) = transform e
                                                         ; return (ISeqn $ toList seq)
                                                         }
compStmt (Return e)             = tryExpr [Int] e $ do { let (seq, var) = transform e
                                                       ; let stmt = toList $ seq |> (IReturn var)
                                                       ; return (ISeqn stmt)
                                                       }

tryExpr                         :: [Type] -> Expr -> ST IStmt -> ST IStmt
tryExpr t e a                   = if validExpression t e 
                                        then a
                                        else do writeError $ "Invalid expression type near " ++ show (getExprSrc e)
                                                return (ISeqn [])
                                                
                                     
compAssign                      :: Name -> Expr -> ST IStmt
compAssign n (Val _ i)          = return (IAssign n (IVal i))
compAssign n (Var pos v)        = do posV <- getEnvVar v pos
                                     return (IAssign n (IVar v))
compAssign _ (Lit pos _)        = do writeError $ "Invalid assignment near " ++ show pos
                                     return (ISeqn [])
compAssign _ (Compare p _ _ _)  = do writeError $ "Invalid assignment near " ++ show p
                                     return (ISeqn [])
compAssign n expr               = do let (seq, var) = transform expr
                                     let stmt = toList $ seq |> (IAssign n var)
                                     return (ISeqn stmt)
                                     
validExpression                 :: [Type] -> Expr -> Bool
validExpression t e             = (parseType e) `elem` t

parseType                       :: Expr -> Type
parseType (Val _ _)             = Int
parseType (Var _ _)             = Int
parseType (Lit _ _)             = Str
parseType (Compare _ _ e1 e2)   = if (typeAnd (parseType e1) (parseType e2)) == Int then Bool else InvalidType
parseType (App _ _ e1 e2)       = if (typeAnd (parseType e1) (parseType e2)) == Int then Int else InvalidType
parseType (Apply _ _ es)        = if foldl (&&) True (map (validExpression intAndStr) es) then Int else InvalidType

typeAnd                         :: Type -> Type -> Type
typeAnd Str _                   = InvalidType
typeAnd _ Str                   = InvalidType
typeAnd x y                     = if x == y then x else InvalidType

-- helper functions

intAndStr = [Int, Str]
intAndBool = [Int, Bool] 
anyType = [Int, Str, Bool]

getExprSrc                      :: Expr -> SourcePos
getExprSrc (Val p _)            = p
getExprSrc (Var p _)            = p
getExprSrc (Lit p _)            = p
getExprSrc (Compare p _ _ _)    = p
getExprSrc (App p _ _ _)        = p
getExprSrc (Apply p _ es)       = p                          