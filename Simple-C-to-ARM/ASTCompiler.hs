
module ASTCompiler (
     compE
) where

import Prelude hiding (EQ)
import Data.Foldable (toList)
import Data.Sequence
import Text.Parsec.Pos
import Control.Monad
import Data.List
import AST
import IAST
import Environment
import DAG
import VMInst
import Extra

compE                           :: Prog -> IO IProg
compE p                         = case err of
                                        [] -> return prog
                                        xs -> do mapM_ putStrLn xs
                                                 return (IPSeq [])
                                   where (prog, (_, err, _)) = runState' p
                                              
--runState' p = runState (compProg p) (emptyTop 0, [], 0)
runState' p = runState (start p) (emptyTop 0, [], 0)                                              
type Error = [String]

data Info = VarL Type | Func Type Int

-- State Monad 
-- ===========

-- Declaration for the state monad and a new type runState to save writing State -(a, State).

data ST a     = S { runState :: State -> (a, State) }
type State    = (Env Name Info Int, Error, Int)

apply         :: ST a -> State -> (a,State)
apply (S f)  = f 

instance Monad ST where
      -- return :: a -> ST a
      return x   = S (\s -> (x,s))

      -- (>>=)  :: ST a -> (a -> ST b) -> ST b
      st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')


      
getN                    :: ST Int
getN                    =  S (\(env, e, n) -> (n, (env, e, n + 1)))

setN                    :: Int -> ST ()
setN n                  =  S (\(env, e, _) -> ((), (env, e, n)))

writeError              :: String -> ST ()
writeError e            =  S (\(env, err, n) -> ((), (env, e:err, n)))

addEnvLevel             :: ST ()
addEnvLevel             =  S (\(env, e, n) -> ((), (addLevel env 0, e, n)))

remEnvLevel             :: ST ()
remEnvLevel             =  S (\(env, e, n) -> ((), (removeLevel env, e, n)))

addEnvVar               :: Name -> Info -> ST ()
addEnvVar q t           =  S (\(env, e, n) -> ((), (env `addVar` (q, t), e, n)))

getEnvVar                :: Name -> SourcePos -> ST Info
getEnvVar q p            = do env <- getEnv 
                              case env `getVar` q of 
                                                Nothing -> if "$" `isPrefixOf` q 
                                                                then do let _:name = q
                                                                        writeError $ "undefined function " ++ show name ++ " near: " ++ show p
                                                                        return (Func Int 0)
                                                                else do writeError $ "undefined variable " ++ show q ++ " near: " ++ show p
                                                                        return (VarL Int)
                                                Just a  -> return a

hasEnvVar               :: Name -> SourcePos -> ST ()
hasEnvVar q p           = do env <- getEnv 
                             case env `getVar` q of 
                                        Nothing -> return ()
                                        Just _  -> if "$" `isPrefixOf` q 
                                                        then do let _:name = q
                                                                writeError $ "double declaration of function " ++ show name ++ " near: " ++ show p
                                                        else writeError $ "double declaration of " ++ show q ++ " near: " ++ show p
                                                
getEnv                  :: ST (Env Name Info Int)
getEnv                  =  S (\(env, e, n) -> (env, (env, e, n)))

start                           :: Prog -> ST IProg
start p                         = do addEnvVar ("$printf") (Func Int $ -1)
                                     --addEnvVar ("$print") (Func Int $ -1)
                                     addEnvVar ("$read") (Func Int $ -1)
                                     prog <- compProg p
                                     getEnvVar "$main" (initialPos "")
                                     return prog

-- [names of arguments] -> where should it start
addFuncArgs             :: [Name] -> ST ()
addFuncArgs []          = return ()
addFuncArgs (n:ns)      = do addEnvVar n (VarL Int)
                             addFuncArgs ns

compProg                        :: Prog -> ST IProg
compProg (GlobalVar n pos)      = return (IGlobalVar n)
compProg (Fun n ns st pos)      = do hasEnvVar ('$':n) pos
                                     addEnvVar ('$':n) (Func Int (Data.List.length ns))
                                     addEnvLevel
                                     addFuncArgs ns
                                     stmt <- compStmt st
                                     remEnvLevel
                                     return (IFun n ns stmt Empt)
compProg (PSeq [])              = return (IPSeq [])
compProg (PSeq xs)              = do list <- mapM compProg xs
                                     return (IPSeq list)
                                     
                
compStmt                        :: Stmt -> ST IStmt
compStmt (LocalVar n src)       = do hasEnvVar n src
                                     addEnvVar n (VarL Int)
                                     return (ILocalVar n Empt)
compStmt (Assign pos n e)       = do posV <- getEnvVar n pos
                                     compAssign n e
compStmt (Print str e)          = do let bools = map (validExpression [Int]) e
                                     if and bools then do { n <- getN
                                                          ; (seq, var, n') <- transforM' e n
                                                          ; setN n'
                                                          ; let stmt = toList $ seq |> (IPrint str var Empt)
                                                          ; return (ISeqn stmt)
                                                          }
                                                  else return $ ISeqn []
compStmt (Seqn  [])             = return (E_STMT)
compStmt (SeqnE [])             = return (E_STMT)
compStmt (Seqn  xs)             = do list <- mapM compStmt xs
                                     return (ISeqn list)
compStmt (SeqnE xs)             = do addEnvLevel
                                     list <- mapM compStmt xs
                                     remEnvLevel
                                     return (ISeqnE list Empt)
compStmt (While e p)            = tryExpr intAndBool e $ do { n <- getN
                                                            ; (seq, var, n') <- transform' e n
                                                            ; setN n'
                                                            ; let stmt = toList seq 
                                                            ; p' <- compStmt p
                                                            --; return (IWhile stmt var (ISeqnE (toList (p' <| seq)) Empt) Empt Empt)
                                                            ; return (IWhile stmt var (ISeqnE (addStmts p' stmt) Empt) Empt Empt)
                                                            }
compStmt (If e p1 p2)           = tryExpr intAndBool e $ do { p1' <- compStmt p1
                                                            ; p2' <- compStmt p2
                                                            ; n <- getN
                                                            ; (seq, var, n') <- transform' e n
                                                            ; setN n'
                                                            ; let stmt = toList $ seq |> (IIf var p1' p2' Empt)
                                                            ; return (ISeqn stmt)
                                                            }
compStmt (Ex e)                 = tryExpr anyType e $ do { n <- getN
                                                         ; (seq, var, n') <- transform' e n
                                                         ; setN n'
                                                         ; return (ISeqn $ toList seq)
                                                         }
compStmt (Return e)             = tryExpr [Int] e $ do { n <- getN
                                                       ; (seq, var, n') <- transform' e n
                                                       ; setN n'
                                                       ; let stmt = toList $ seq |> (IReturn var Empt)
                                                       ; return (ISeqn stmt)
                                                       }
compStmt (Break pos)            = return $ IBreak pos

tryExpr                         :: [Type] -> Expr -> ST IStmt -> ST IStmt
tryExpr t e a                   = if validExpression t e 
                                        then a
                                        else do writeError $ "Invalid expression type near " ++ show (getExprSrc e)
                                                return (ISeqn [])
                                                
addStmts                        :: IStmt -> [IStmt] -> [IStmt]
addStmts (ISeqnE xs e) ys       = (xs ++ ys)
addStmts stmt        ys         = (stmt:ys)
                                                
compAssign                      :: Name -> Expr -> ST IStmt
compAssign n (Val _ i)          = return (IAssign n (IVal i) Empt)
compAssign n (Var pos v)        = do posV <- getEnvVar v pos
                                     return (IAssign n (IVar v) Empt)
compAssign _ (Lit pos _)        = do writeError $ "Invalid assignment near " ++ show pos
                                     return (ISeqn [])
compAssign n (Read _)           = return (IRead n Empt)
compAssign _ (Compare p _ _ _)  = do writeError $ "Invalid assignment near " ++ show p
                                     return (ISeqn [])
compAssign name expr            = do n <- getN
                                     (seq, var, n') <- transform' expr n
                                     setN n'
                                     let stmt = toList $ seq |> (IAssign name var Empt)
                                     return (ISeqn stmt)

transform'                      :: Expr -> Int -> ST (Seq IStmt, Value, Int)
transform' e i                  = do valid <- validExpressionVar e
                                     case valid of 
                                        True -> return (transform e i)
                                        False -> return (empty, IVal 0, 0)

transforM'                      :: [Expr] -> Int -> ST (Seq IStmt, [Value], Int)
transforM' [    ] i             = return (empty, [], i)
transforM' (e:es) i             = do valid <- validExpressionVar e
                                     case valid of 
                                        True -> do let (seq, v, i') = transform e i
                                                   (seq', vs, i'') <- transforM' es i'
                                                   return (seq >< seq', v:vs, i'')
                                        False -> transforM' es i

validExpressionVar                      :: Expr -> ST Bool
validExpressionVar (Val _ _)            = return True
validExpressionVar (Var src n)          = do env <- getEnv 
                                             case env `getVar` n of 
                                                Nothing -> do writeError $ "undefined variable " ++ show n ++ " near: " ++ show src
                                                              return False
                                                Just _  -> return True
validExpressionVar (Lit _ _)            = return True
validExpressionVar (Read _ )            = return True
validExpressionVar (Compare _ _ e1 e2)  = do b1 <- validExpressionVar e1
                                             b2 <- validExpressionVar e2
                                             return (b1 && b2)
validExpressionVar (App _ _ e1 e2)      = do b1 <- validExpressionVar e1
                                             b2 <- validExpressionVar e2
                                             return (b1 && b2)
validExpressionVar (Apply src n es)     = do Func _ i <- getEnvVar ('$':n) src
                                             let len = Data.List.length es
                                             let start = len == i
                                             if start then return ()
                                                      else writeError $ "function " ++ show n ++ " takes " ++ show i ++
                                                                        " arguments (passed " ++ show len ++ ") near: " ++ show src
                                             let f False _ = return False
                                                 f True  e = validExpressionVar e
                                             b <- foldM f start es
                                             return b
                                             

validExpression                 :: [Type] -> Expr -> Bool
validExpression t e             = (parseType e) `elem` t

parseType                       :: Expr -> Type
parseType (Val _ _)             = Int
parseType (Var _ _)             = Int
parseType (Lit _ _)             = Str
parseType (Read _ )             = Int
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
getExprSrc (Read p )            = p
getExprSrc (Compare p _ _ _)    = p
getExprSrc (App p _ _ _)        = p
getExprSrc (Apply p _ es)       = p                          