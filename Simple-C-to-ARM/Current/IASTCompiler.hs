
module IASTCompiler (
     comp,
     compI
) where

import Prelude hiding (EQ)
import Data.Foldable (toList)
import Data.Sequence
import Text.Parsec.Pos
import AST
import IAST
import Environment
import VMInst

comp                            :: IProg -> Code 
comp p                          = toList code
                                        where (_, (_, _, err, code)) = runState' p

compI                           :: IProg -> IO Code
compI p                         = case err of
                                        [] -> return $ toList code
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

addTempVar              :: Name -> ST()
addTempVar q            = do env <- getEnv
                             case env `getVarCLevel` q of 
                                        Nothing -> do dis <- getEnvDisplacement
                                                      addEnvVar q (P SB dis)
                                                      addEnvDisplacement 1
                                                      emit (ADD SP SP (VAL 1))
                                        Just p  -> return ()

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
addFuncArgs             :: [Name] -> Integer -> ST ()
addFuncArgs [] _        = return ()
addFuncArgs (n:ns) i    = do addEnvVar n (P SB (-i))
                             addFuncArgs ns (i + 1)

compProg                        :: IProg -> ST ()
compProg (IGlobalVar n)         = do addEnvVar n (P (G n) 0)
                                     emit       (ADDRESS n)
compProg (IFun n ns st)         = do addEnvLevel
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
compProg (IPSeq [    ])         = return ()
compProg (IPSeq (x:xs))         = do compProg x
                                     compProg (IPSeq xs)
                

--compFun (n ns st)
                
                
compStmt                        :: IStmt -> ST ()
compStmt (ILocalVar n)          = do dis <- getEnvDisplacement
                                     addEnvVar n (P SB dis)
                                     addEnvDisplacement 1
                                     emit       (ADD SP SP (VAL 1))
compStmt (IAssign v val)        = compAssign v val
compStmt (IPrint val)           = do reg <- compVal val TEMP
                                     emit       (PRINT reg)
compStmt (ISeqn [    ])         = return ()
compStmt (ISeqn (x:xs))         = do compStmt x
                                     compStmt (ISeqn xs)
compStmt (IWhile es v p)        = do lb <- fresh
                                     lb' <- fresh
                                     emit (LABEL lb) 
                                     compStmts es 
                                     jumpz v lb'
                                     compStmt p
                                     emitCode   [B NONE lb, LABEL lb']
compStmt (IIf v p1 (ISeqn []))  = do lb <- fresh
                                     jumpz v lb
                                     compStmt p1
                                     emit (LABEL lb)
compStmt (IIf v p1 p2)          = do lb <- fresh
                                     lb' <- fresh
                                     jumpz v lb
                                     compStmt p1 
                                     emitCode [B NONE lb', LABEL lb] 
                                     compStmt p2 
                                     emit (LABEL lb')
compStmt (IReturn v)            = do reg <- compVal v (R "r0")
                                     return ()
compStmt (IApply n vals res)    = do addTempVar res
                                     prepareFunctionCall vals
                                     emit (BL NONE (N n))
compStmt (IApp n op v1 v2)      = do addTempVar n
                                     r1 <- compVal v1 (R "r11")
                                     r2 <- compVal v2 TEMP
                                     case op of Add -> emit (ADD TEMP r1 (P r2 0))
                                                Sub -> emit (SUB TEMP r1 (P r2 0))
                                                Mul -> emit (MUL TEMP r1 (P r2 0))
                                                Div -> emit (SUB TEMP r1 (P r2 0))
                                     posV <- getEnvVar n
                                     emit (STR TEMP posV)

              
compAssign                      :: Name -> Value -> ST ()
compAssign v val                = do posV <- getEnvVar v
                                     reg <- compVal val TEMP
                                     emit (STR reg posV)


compVal                         :: Value -> Reg -> ST Reg
compVal (IVal i) reg            = do emit (MOV reg (VAL i))
                                     return reg
compVal (IVar n) reg            = do posVar <- getEnvVar n
                                     emit (LDR reg posVar)
                                     return reg
compVal LastReturn _            = return (R "r0")


prepareFunctionCall             :: [Value] -> ST ()
prepareFunctionCall []          = return ()
prepareFunctionCall (v:vs)      = do reg <- compVal v TEMP
                                     emit (PUSHV reg)
                                     prepareFunctionCall vs
                                        
              
jumpz                           :: Value -> Label -> ST ()
jumpz (IVal i) lb               = if i == 0 then emit (B NONE lb) else return ()
jumpz (IVar v) lb               = do posV <- getEnvVar v
                                     emit (LDR TEMP posV)
                                     emit (CMP TEMP (VAL 0))
                                     emit (B EQ lb)
jumpz (LastReturn) lb           = do emit (CMP (R "r0") (VAL 0))
                                     emit (B EQ lb)

                                     
compStmts                     :: [IStmt] -> ST()
compStmts []                  = return ()
compStmts (e:es)              = do compStmt e
                                   compStmts es
                                     