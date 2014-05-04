
module IASTCompiler (
     comp,
     compI
) where

import Prelude hiding (EQ, LT, GT)
import Data.Foldable (toList)
import Data.Sequence
import Data.List
import Text.Parsec.Pos
import AST
import IAST
import VMInst
import Helper
import Environment

comp                            :: IProg -> Code 
comp p                          = toList code
                                        where (_, (_, _, err, code)) = runState' p

compI                           :: IProg -> IO Code
compI p                         = case err of
                                        [] -> return $ toList code
                                        xs -> do print xs
                                                 return []
                                   where (_, (_, _, err, code)) = runState' p
                                              
runState' p = runState (compProg p) (0, emptyTop registers, [], empty)                                              

type Error = [String]

data VarPos = NoPos | Pos Imd

data VarReg = NoReg | Rreg Reg

data Info = Inf VarReg VarPos Int

data LevelInfo = Inf2 Registers [(Reg, Imd)]

data VarInfo = Inf3 Name Int

-- State Monad 
-- ===========

-- Declaration for the state monad and a new type runState to save writing State -(a, State).

data ST a     = S { runState :: State -> (a, State) }
type State    = (Integer, Env Name Info LevelInfo, Error, Seq Inst, Env Reg VarInfo Int)

apply         :: ST a -> State -> (a,State)
apply (S f)  = f 

instance Monad ST where
      -- return :: a -> ST a
      return x   = S (\s -> (x,s))

      -- (>>=)  :: ST a -> (a -> ST b) -> ST b
      st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')

      
-- The function that generates the fresh labels. It is of type ST showing that it has a hidden state. 

fresh                   :: ST Label
fresh                   =  S (\(n, env, e, c, regEnv) -> (V n, (n+1, env, e, c, regEnv)))

emit                    :: Inst -> ST ()
emit i                  = S (\(n, env, e, c, regEnv) -> ((), (n, env, e, c |> i, regEnv)))

emitCode                :: Code -> ST ()
emitCode c'             = S (\(n, env, e, c, regEnv) -> ((), (n, env, e, c >< fromList c', regEnv)))

writeError              :: String -> ST ()
writeError e            =  S ((\(n, env, err, c, regEnv) -> ((), (n, env, e:err, c, regEnv))))

addEnvLevel             :: ST ()
addEnvLevel             =  S (\(n, env, e, c, regEnv) -> ((), (n, addLevel env, e, c, regEnv)))

remEnvLevel             :: ST ()
remEnvLevel             =  S (\(n, env, e, c, regEnv) -> ((), (n, removeLevel env, e, c, regEnv)))

addEnvVar               :: Name -> Info -> ST ()
addEnvVar q i           =  S (\(n, env, e, c, regEnv) -> ((), (n, env `addVar` (q, i), e, c, regEnv)))

setEnvDisplacement      :: Integer -> ST ()
setEnvDisplacement i    =  S (\(n, env, e, c, regEnv) -> ((), (n, env `setDisplacement` i, e, c, regEnv)))

getEnvDisplacement      :: ST Integer
getEnvDisplacement      =  S (\(n, env, e, c, regEnv) -> (displacement env, (n, env, e, c, regEnv)))

addEnvDisplacement      :: Integer -> ST ()
addEnvDisplacement i    =  S (\(n, env, e, c, regEnv) -> ((), (n, env `addDisplacement` i, e, c, regEnv)))

copyEnvExtra            :: Env Info -> ST ()
copyEnvExtra oEnv       =  S (\(n, env, e, c, regEnv) -> ((), (n, env `copyExtra` oEnv, e, c, regEnv)))

getEnv                  :: ST (Env Name Info LevelInfo)
getEnv                  =  S (\(n, env, e, c, regEnv) -> (env, (n, env, e, c, regEnv)))

getRegEnv               :: ST (Env Reg VarInfo Int)
getRegEnv               =  S (\(n, env, e, c, regEnv) -> (regEnv, (n, env, e, c, regEnv)))

removeFuncReg           :: Reg -> ST ()
removeFuncReg r         =  S (\(n, env, e, c, regEnv) -> let Inf2 (Rs fReg a) b = getExtra env in
                                                         let fReg' = delete r fReg in
                                                         ((), (n, env `setExtra` (Inf2 (Rs fReg' a) b), e, c, regEnv)))

removeGeneralReg        :: Reg -> ST ()
removeGeneralReg r      =  S (\(n, env, e, c, regEnv) -> let Inf2 (Rs a gReg) b = getExtra env in
                                                         let gReg' = delete r gReg in
                                                         ((), (n, env `setExtra` (Inf2 (Rs a gReg') b), e, c, regEnv)))


addTempVar              :: Name -> ST()
addTempVar q            = if isPrefixOf "@" q 
                             then do env <- getEnv
                                     case env `getVarCLevel` q of 
                                        Nothing -> do dis <- getEnvDisplacement
                                                      addEnvVar q (P SB dis)
                                                      addEnvDisplacement 1
                                                      emit (ADD SP SP (VAL 1))
                                        Just p  -> return ()
                             else return ()

getEnvVar               :: Name -> ST Info
getEnvVar q             = do env <- getEnv 
                             case env `getVar` q of 
                                        Nothing -> do writeError $ "internal error: could not find " ++ (show q)
                                                      return (Inf NoReg NoPos 0)
                                        Just p  -> return p
                                                   

getRegMin               :: Int -> ST (Reg, Name)
getRegMin i             = do regEnv <- getRegEnv
                             let m = getMap regEnv
                             let (r, Inf3 n _) = minimumBy comp (M.toList m)
                                        where
                                                comp (_, Inf3 _ x) (_, Inf3 _ y) | x > i     = Prelude.LT
                                                                                 | y > i     = Prelude.GT
                                                                                 | otherwise = compare x y
                             return (r, n)
                             
                                                   
-- name of variable, register to be assigned to, extra info
newInfoReg              :: Name -> Reg -> Extra -> ST Info
newInfoReg n r e        = do removeFuncReg r
                             case e `getVarNumber` n of
                                Nothing -> return (Inf r NoPos 0)
                                Just a  -> return (Inf r NoPos a)
                                
newInfo                 :: Name -> Extra -> Info
newInfo n e             = case e `getVarNumber` n of
                                Nothing -> Inf NoReg NoPos 0
                                Just a  -> Inf NoReg NoPos a
                                
newInfoStack            :: Name -> Integer -> Extra -> Info
newInfostack n i e      = case e `getVarNumber` n of
                                Nothing -> return (Inf NoReg (P SB (-i)) 0)
                                Just a  -> return (Inf NoReg (P SB (-i)) a)
                                                
-- [names of arguments] -> where should it start
addFuncArgs             :: [Name] -> Integer -> Extra -> ST ()
addFuncArgs [] _        = return ()
addFuncArgs (n:ns) i e  
           | i <= 3     = do info <- newInfoReg n (R $ "r" ++ show i) e
                             addEnvVar n info
                             addFuncArgs ns (i + 1)
           | otherwise  = do let info = newInfoStack n (i - 3) e
                             addEnvVar n info
                             addFuncArgs ns (i + 1)

compProg                        :: IProg -> ST ()
compProg (IGlobalVar n)         = do info <- newInfoReg n (G n) Empt
                                     addEnvVar n info
                                     emit       (ADDRESS n)
compProg (IFun n ns st e)       = do addEnvLevel
                                     --prepare the environment for function arguments
                                     addFuncArgs ns 0 e
                                     setEnvDisplacement 2
                                     --save SB, LR and execute the code and increment SP by the number of args.
                                     emitCode   [LABEL (N n), PUSHV SB, MOV SB (P SP 0), PUSHV LR]
                                     compStmt st
                                     envDis <- getEnvDisplacement
                                     emitCode   [SUB SP SP (VAL (envDis - 2)), POP LR, POP SB]
                                     emit       (SUB SP SP (VAL (toInteger(Prelude.length ns))))
                                     emitCode   [BX NONE LR]
                                     -- restore SP to before arguments
                                     remEnvLevel
compProg (IPSeq xs)             = mapM_ compProg xs
                
                
compStmt                        :: IStmt -> ST ()
compStmt (ILocalVar n e)        = do let info = newInfo n e
                                     addEnvVar n info
                                     --addEnvDisplacement 1
                                     --emit       (ADD SP SP (VAL 1))
compStmt (IAssign v val e)      = do posV <- getEnvVar v
                                     reg <- compVal val TEMP
                                     emit (STR reg posV)
compStmt (IPrint val e)         = do reg <- compVal val TEMP
                                     emit       (PRINT reg)
compStmt (E_STMT)               = return ()
compStmt (ISeqn  xs)            = mapM_ compStmt xs
compStmt (ISeqnE xs)            = do dis <- getEnvDisplacement
                                     addEnvLevel
                                     setEnvDisplacement dis
                                     mapM_ compStmt xs
                                     dis2 <- getEnvDisplacement
                                     remEnvLevel
                                     emit (SUB SP SP (VAL (dis2 - dis)))
compStmt (IWhile es v p e)      = do lb <- fresh
                                     lb' <- fresh
                                     emit (LABEL lb) 
                                     mapM_ compStmt es 
                                     jumpz v lb'
                                     compStmt p
                                     emitCode   [B NONE lb, LABEL lb']
compStmt (IIf v p1 E_STMT e)    = do lb <- fresh
                                     jumpz v lb
                                     compStmt p1
                                     emit (LABEL lb)
compStmt (IIf v p1 p2 e)        = do lb <- fresh
                                     lb' <- fresh
                                     jumpz v lb
                                     compStmt p1 
                                     emitCode [B NONE lb', LABEL lb] 
                                     compStmt p2 
                                     emit (LABEL lb')
compStmt (IReturn v e)          = do reg <- compVal v (R "r0")
                                     return ()
compStmt (IApply n vals res e)  = do addTempVar res -- result
                                     prepareFunctionCall vals
                                     emit (BL NONE (N n))
compStmt (IApp n op v1 v2 e)    = do addTempVar n
                                     r1 <- compVal v1 (R "r11")
                                     r2 <- compVal v2 TEMP
                                     case op of Add -> emit (ADD TEMP r1 (P r2 0))
                                                Sub -> emit (SUB TEMP r1 (P r2 0))
                                                Mul -> emit (MUL TEMP r1 (P r2 0))
                                                Div -> emit (SUB TEMP r1 (P r2 0))
                                     posV <- getEnvVar n
                                     emit (STR TEMP posV)


compVal                         :: Value -> ST Reg
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
jumpz (IComp c v1 v2) lb        = do r1 <- compVal v1 (R "r11")
                                     r2 <- compVal v2 TEMP
                                     emit (CMP r1 (P r2 0))
                                     emit (B (condOpposite c) lb)
                                     
compStmts                     :: [IStmt] -> ST()
compStmts []                  = return ()
compStmts (e:es)              = do compStmt e
                                   compStmts es
                                     
                                     
condOpposite            :: Cond -> Cond
condOpposite EQ         = NE          
condOpposite NE         = EQ
condOpposite GT         = LE
condOpposite LT         = GE
condOpposite GE         = LT
condOpposite LE         = GT