
module IASTCompiler (
     comp,
     compI
) where

import Prelude
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Sequence
import Data.List
import Data.Maybe
import Text.Parsec.Pos
import AST
import IAST
import VMInst
import Helper
import Environment
import Extra

comp                            :: IProg -> Code 
comp p                          = case err of 
                                        [] -> toList code
                                        xs -> [PUSHV (R (concat xs))]
                                        where (_, (_, _, err, code, _)) = runState' p

compI                           :: IProg -> IO Code
compI p                         = case err of
                                        [] -> return $ toList code
                                        xs -> do print xs
                                                 return []
                                   where (_, (_, _, err, code, _)) = runState' p
                                              
runState' p = runState (compProg p) (0, emptyTop 0, [], empty, emptyTop (Inf2 registers [] []))                                              

type Error = [String]

data VarPos = Pos Imd

data VarReg = NoReg | Rreg Reg

data Info = Inf VarReg VarPos Int

--Inf2 (all registers) (vars that use func reg) (used general registers)
data LevelInfo = Inf2 Registers [Name] [(Reg, Imd)]

data VarInfo = Inf3 Name Int

-- State Monad 
-- ===========

-- Declaration for the state monad and a new type runState to save writing State -(a, State).

data ST a     = S { runState :: State -> (a, State) }
type State    = (Integer, Env Name Info Int, Error, Seq Inst, Env Reg VarInfo LevelInfo)

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
addEnvLevel             =  S (\(n, env, e, c, regEnv) -> ((), (n, addLevel env 0, e, c, regEnv)))

addRegLevel             :: ST ()
addRegLevel             =  S (\(n, env, e, c, regEnv) -> ((), (n, env, e, c, addLevel regEnv (Inf2 registers [] []))))

remEnvLevel             :: ST ()
remEnvLevel             =  S (\(n, env, e, c, regEnv) -> ((), (n, removeLevel env, e, c, regEnv)))

remRegLevel             :: ST ()
remRegLevel             =  S (\(n, env, e, c, regEnv) -> ((), (n, env, e, c, removeLevel regEnv)))

addEnvVar               :: Name -> Info -> ST ()
addEnvVar q i           =  S (\(n, env, e, c, regEnv) -> ((), (n, env `addVar` (q, i), e, c, regEnv)))

setEnvVar               :: Name -> Info -> ST ()
setEnvVar q i           =  S (\(n, env, e, c, regEnv) -> ((), (n, env `setVar` (q, i), e, c, regEnv)))

addRegInfo              :: Reg -> VarInfo -> ST ()
addRegInfo r i          =  S (\(n, env, e, c, regEnv) -> ((), (n, env, e, c, regEnv `addVar` (r, i))))

setEnvDisplacement      :: Integer -> ST ()
setEnvDisplacement i    =  S (\(n, env, e, c, regEnv) -> ((), (n, env `setDisplacement` i, e, c, regEnv)))

getEnvDisplacement      :: ST Integer
getEnvDisplacement      =  S (\(n, env, e, c, regEnv) -> (displacement env, (n, env, e, c, regEnv)))

addEnvDisplacement      :: Integer -> ST ()
addEnvDisplacement i    =  S (\(n, env, e, c, regEnv) -> ((), (n, env `addDisplacement` i, e, c, regEnv)))

getEnv                  :: ST (Env Name Info Int)
getEnv                  =  S (\(n, env, e, c, regEnv) -> (env, (n, env, e, c, regEnv)))

getRegEnv               :: ST (Env Reg VarInfo LevelInfo)
getRegEnv               =  S (\(n, env, e, c, regEnv) -> (regEnv, (n, env, e, c, regEnv)))

setRegExtra             :: LevelInfo -> ST ()
setRegExtra info        =  S (\(n, env, e, c, regEnv) -> ((), (n, env, e, c, regEnv `setExtra` info)))


addTempVar              :: Name -> Extra -> ST()
addTempVar n e          = if isPrefixOf "@" n
                             then do env <- getEnv
                                     case env `getVarCLevel` n of 
                                        Nothing -> do info <- newInfo n e
                                                      addEnvVar n info
                                        Just p  -> return ()
                             else return ()

getEnvVar               :: Name -> ST Info
getEnvVar q             = do env <- getEnv 
                             case env `getVar` q of 
                                        Nothing -> do writeError $ "internal error: could not find " ++ (show q)
                                                      return (Inf NoReg (Pos (P SB 0)) 0)
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

getRegVar               :: Int -> ST (Maybe Reg)
getRegVar i             = do regEnv <- getRegEnv
                             let m = getMap regEnv
                             let f (_, Inf3 _ x) | x > i     = True
                                                 | otherwise = False
                             case listToMaybe (Data.List.filter f (M.toList m)) of
                                Nothing         -> return Nothing
                                Just (r, _)     -> return $ Just r

getRegList              :: ST (Maybe Reg)
getRegList              = do env <- getRegEnv
                             let Inf2 (Rs a gReg) ns b = getExtra env
                             case listToMaybe gReg of
                                Nothing -> return Nothing
                                Just r  -> do setRegExtra (Inf2 (Rs a (tail gReg)) ns b)
                                              return (Just r)
                          
getReg                  :: Extra -> ST Reg
getReg (I1 _ i)         = do reg1 <- getRegVar i
                             case reg1 of
                                Nothing -> do reg2 <- getRegList
                                              case reg2 of
                                                Nothing -> do (reg3, name) <- getRegMin i
                                                              saveVar name
                                                              return reg3
                                                Just r  -> do saveRegVal r
                                                              return r
                                Just r  -> return r

saveRegVal              :: Reg -> ST ()
saveRegVal r            = do env <- getRegEnv
                             let Inf2 (Rs a b) ns regs = getExtra env
                             dis <- getEnvDisplacement
                             addEnvDisplacement 1
                             emit       (STR r (P SB dis))
                             emit       (ADD SP SP (VAL 1))
                             setRegExtra (Inf2 (Rs a b) ns ((r, P SB dis):regs))

restoreRegisters        :: ST ()
restoreRegisters        = do env <- getRegEnv
                             let Inf2 (Rs a b) ns regs = getExtra env
                             mapM_ f regs
                                where f (r, imd) = emit (LDR r imd)
                             

saveVar'                                :: Name -> Info -> ST ()
saveVar' _ (Inf NoReg _ _)              = return ()
saveVar' n (Inf (Rreg r) (Pos imd) i)   = do emit       (STR r imd)
                                             setEnvVar n (Inf NoReg (Pos imd) i)

saveVar                 :: Name -> ST ()
saveVar n               = do env <- getEnv
                             info <- getEnvVar n
                             saveVar' n info

saveFuncRegVars         :: ST ()
saveFuncRegVars         = do env <- getRegEnv 
                             let Inf2 (Rs a b) ns c = getExtra env
                             mapM_ saveVar ns
                             setRegExtra (Inf2 (Rs a b) [] c)
                                

removeFuncReg           :: Reg -> Name -> ST ()
removeFuncReg r n       =  do env <- getRegEnv 
                              let Inf2 (Rs fReg a) ns b = getExtra env
                              let fReg' = delete r fReg
                              setRegExtra (Inf2 (Rs fReg' a) (n:ns) b)
                              
removeGeneralReg        :: Reg -> ST ()
removeGeneralReg r      =  do env <- getRegEnv 
                              let Inf2 (Rs a gReg) ns b = getExtra env
                              let gReg' = delete r gReg
                              setRegExtra (Inf2 (Rs a gReg') ns b)
                          
-- name of variable, register to be assigned to, extra info
newInfoReg              :: Name -> Reg -> Extra -> ST Info
newInfoReg n r e        = do removeFuncReg r n
                             dis <- getEnvDisplacement
                             addEnvDisplacement 1
                             let imd = P SB dis
                             emit       (ADD SP SP (VAL 1))
                             case e `getVarNumber` n of
                                Nothing -> return (Inf (Rreg r) (Pos imd) 0)
                                Just a  -> return (Inf (Rreg r) (Pos imd) a)
                                
newInfo                 :: Name -> Extra -> ST Info
newInfo n e             = do dis <- getEnvDisplacement
                             addEnvDisplacement 1
                             let imd = P SB dis
                             emit       (ADD SP SP (VAL 1))
                             case e `getVarNumber` n of
                                Nothing -> return (Inf NoReg (Pos imd) 0)
                                Just a  -> return (Inf NoReg (Pos imd) a)
                                
newInfoStack            :: Name -> Integer -> Extra -> Info
newInfoStack n i e      = case e `getVarNumber` n of
                                Nothing -> (Inf NoReg (Pos $ P SB (-i)) 0)
                                Just a  -> (Inf NoReg (Pos $ P SB (-i)) a)
                                                
-- [names of arguments] -> where should it start
addFuncArgs             :: [Name] -> Integer -> Extra -> ST ()
addFuncArgs [] _ e      = return ()
addFuncArgs (n:ns) i e  
           | i <= 3     = do info <- newInfoReg n (R $ "r" ++ show i) e
                             addEnvVar n info
                             addFuncArgs ns (i + 1) e
           | otherwise  = do let info = newInfoStack n (i - 3) e
                             addEnvVar n info
                             addFuncArgs ns (i + 1) e

compProg                        :: IProg -> ST ()
compProg (IGlobalVar n)         = do info <- newInfoReg n (G n) Empt
                                     addEnvVar n info
                                     emit       (ADDRESS n)
compProg (IFun n ns st e)       = do addEnvLevel
                                     addRegLevel
                                     --prepare the environment for function arguments
                                     addFuncArgs ns 0 e
                                     setEnvDisplacement 2
                                     --save SB, LR and execute the code and increment SP by the number of args.
                                     emitCode   [LABEL (N n), PUSHV SB, MOV SB (P SP 0), PUSHV LR]
                                     compStmt st
                                     envDis <- getEnvDisplacement
                                     restoreRegisters
                                     emitCode   [SUB SP SP (VAL (envDis - 2)), POP LR, POP SB]
                                     emit       (SUB SP SP (VAL (toInteger(Prelude.length ns))))
                                     emitCode   [BX NONE LR]
                                     -- restore SP to before arguments
                                     remEnvLevel
                                     remRegLevel
compProg (IPSeq xs)             = mapM_ compProg xs
                
                
compStmt                        :: IStmt -> ST ()
compStmt (ILocalVar n e)        = do info <- newInfo n e
                                     addEnvVar n info
compStmt (IAssign n val e)      = do r1 <- compName n e
                                     compVal' val r1
compStmt (IPrint val e)         = do saveFuncRegVars
                                     compVal' val (R "r1")
                                     emit       (PRINT)
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
                                     mapM_ compStmt es 
                                     jumpz v lb' e
                                     emit (LABEL lb)
                                     compStmt p
                                     mapM_ compStmt es 
                                     jumpz v lb' e
                                     emitCode   [B NONE lb, LABEL lb']
compStmt (IIf v p1 E_STMT e)    = do lb <- fresh
                                     jumpz v lb e
                                     compStmt p1
                                     emit (LABEL lb)
compStmt (IIf v p1 p2 e)        = do lb <- fresh
                                     lb' <- fresh
                                     jumpz v lb e
                                     compStmt p1 
                                     emitCode [B NONE lb', LABEL lb] 
                                     compStmt p2 
                                     emit (LABEL lb')
compStmt (IReturn v e)          = compVal' v (R "r0")
compStmt (IApply n vals res e)  = do addTempVar res e -- result
                                     saveFuncRegVars
                                     prepareFunctionCall vals 0
                                     emit (BL NONE (N n))
compStmt (IApp n op v1 v2 e)    = do addTempVar n e
                                     rd <- compName n e
                                     r1 <- compVal v1 e
                                     r2 <- compVal v2 e
                                     case op of Add -> emit (ADD rd r1 (P r2 0))
                                                Sub -> emit (SUB rd r1 (P r2 0))
                                                Mul -> emit (MUL rd r1 (P r2 0))
                                                Div -> emit (SUB rd r1 (P r2 0))


compVal                         :: Value -> Extra -> ST Reg
compVal (IVal i) e              = do reg <- getReg e
                                     emit (MOV reg (VAL i))
                                     return reg
compVal (IVar n) e              = compName n e
compVal LastReturn _            = return (R "r0")

compName                        :: Name -> Extra -> ST Reg
compName n e                    = do Inf reg (Pos imd) _ <- getEnvVar n
                                     case reg of 
                                        NoReg  -> do reg' <- getReg e
                                                     emit (LDR reg' imd)
                                                     case e `getVarNumber` n of
                                                        Nothing -> addRegInfo reg' (Inf3 n 0)
                                                        Just a  -> addRegInfo reg' (Inf3 n a)
                                                     return reg'
                                        Rreg r -> return r

compVal'                        :: Value -> Reg -> ST ()
compVal' (IVal i) reg           = emit (MOV reg (VAL i))
compVal' (IVar n) reg           = do Inf r (Pos imd) _ <- getEnvVar n
                                     case r of
                                        NoReg  -> emit (LDR reg imd)
                                        Rreg r -> if reg /= r then emit (MOV reg (P r 0)) else return ()
compVal' LastReturn reg         = if (R "r0") /= reg then emit (MOV reg (P (R "r0") 0)) else return ()

pushVal                         :: Value -> ST ()
pushVal (IVal i)                = emit (PUSH i)
pushVal (IVar n)                = do Inf r (Pos imd) _ <- getEnvVar n
                                     case r of
                                        NoReg  -> do emit (LDR TEMP imd)
                                                     emit (PUSHV TEMP)
                                        Rreg r -> emit (PUSHV r)
pushVal LastReturn              = emit (PUSHV (R "r0"))

prepareFunctionCall             :: [Value] -> Int -> ST ()
prepareFunctionCall [] _        = return ()
prepareFunctionCall (v:vs) i  
                | i <= 3        = do compVal' v (R $ "r" ++ show i)
                                     prepareFunctionCall vs (i + 1)
                | otherwise     = do pushVal v
                                     prepareFunctionCall vs (i + 1)


jumpz                           :: Value -> Label -> Extra -> ST ()
jumpz (IVal i) lb _             = if i == 0 then emit (B NONE lb) else return ()
jumpz (IVar n) lb e             = do reg <- compName n e
                                     emit (CMP reg (VAL 0))
                                     emit (B AST.EQ lb)
jumpz (LastReturn) lb _         = do emit (CMP (R "r0") (VAL 0))
                                     emit (B AST.EQ lb)
jumpz (IComp c v1 v2) lb e      = do r1 <- compVal v1 e
                                     r2 <- compVal v2 e
                                     emit (CMP r1 (P r2 0))
                                     emit (B (condOpposite c) lb)
                                     
compStmts                     :: [IStmt] -> ST()
compStmts []                  = return ()
compStmts (e:es)              = do compStmt e
                                   compStmts es
                                     
                                     
condOpposite            :: Cond -> Cond
condOpposite AST.EQ     = NE          
condOpposite NE         = AST.EQ
condOpposite AST.GT     = LE
condOpposite AST.LT     = GE
condOpposite GE         = AST.LT
condOpposite LE         = AST.GT