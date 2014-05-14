
module IASTCompiler (
     --comp,
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

debug = False

comp                            :: IProg -> Code 
comp p                          = case err of 
                                        [] -> toList code
                                        xs -> [PUSHV (R (concat xs))]
                                        where (_, (_, _, err, code, _)) = runState' p

compI                           :: IProg -> IO Code
compI p                         = case err of
                                        [] -> return $ toList code
                                        xs -> do mapM_ putStrLn xs
                                                 return []
                                   where (_, (_, _, err, code, _)) = runState' p
                                              
runState' p = runState (compProg p) (0, emptyTop 0, [], empty, emptyTop (Inf2 registers [] []))                                              

type Error = [String]

data VarPos = Pos Imd

data VarReg = NoReg | Rreg Reg

data Info = NoInfo | Inf VarReg VarPos Bool

--Inf2 (all registers) (vars that use func reg) (used general registers)
data LevelInfo = Inf2 Registers [Name] [(Reg, Imd)] deriving Show

data VarInfo = Inf3 Name Int

instance Show VarInfo where
  show (Inf3 n i) = show n ++ ":" ++ show i

-- State Monad 
-- ===========

-- Declaration for the state monad and a new type runState to save writing State -(a, State).

data ST a     = S { runState :: State -> (a, State) }
type State    = (Int, Env Name Info Int, Error, Seq Inst, Env Reg VarInfo LevelInfo)

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
emit (DEBUG s)          = S (\(n, env, e, c, regEnv) -> ((), (n, env, e, if debug then c |> (DEBUG s) else c, regEnv)))
emit i                  = S (\(n, env, e, c, E m1 i1 r1 e1) -> ((), (n, env, e, c |> i, E m1 i1 r1 e1)))

emitCode                :: Code -> ST ()
emitCode c'             = S (\(n, env, e, c, regEnv) -> ((), (n, env, e, c >< fromList c', regEnv)))

writeError              :: String -> ST ()
writeError e            =  S ((\(n, env, err, c, regEnv) -> ((), (n, env, ("internal error:" ++ e):err, c, regEnv))))

addEnvLevel             :: ST ()
addEnvLevel             =  S (\(n, env, e, c, regEnv) -> ((), (n, addLevel env 0, e, c, regEnv)))

addEnvLevel2            :: ST ()
addEnvLevel2            =  S (\(n, E m1 d1 argNr e1, e, c, regEnv) -> ((), (n, E m1 d1 argNr (E m1 d1 argNr e1), e, c, regEnv)))

addRegLevel             :: ST ()
addRegLevel             =  S (\(n, env, e, c, regEnv) -> ((), (n, env, e, c, addLevel regEnv (Inf2 registers [] []))))

addRegLevel2            :: ST ()
addRegLevel2            =  S (\(n, env, e, c, regEnv) -> ((), (n, env, e, c, addLevelCopy regEnv)))

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

setEnvDisplacement      :: Int -> ST ()
setEnvDisplacement i    =  S (\(n, env, e, c, regEnv) -> ((), (n, env `setDisplacement` i, e, c, regEnv)))

getEnvDisplacement      :: ST Int
getEnvDisplacement      =  S (\(n, env, e, c, regEnv) -> (displacement env, (n, env, e, c, regEnv)))

getTotalDisplacement    :: ST Int
getTotalDisplacement    =  S (\(n, env, e, c, regEnv) -> (totalDisplacement env, (n, env, e, c, regEnv)))

addEnvDisplacement      :: Int -> ST ()
addEnvDisplacement i    =  S (\(n, env, e, c, regEnv) -> ((), (n, env `addDisplacement` i, e, c, regEnv)))

getEnv                  :: ST (Env Name Info Int)
getEnv                  =  S (\(n, env, e, c, regEnv) -> (env, (n, env, e, c, regEnv)))

getRegEnv               :: ST (Env Reg VarInfo LevelInfo)
getRegEnv               =  S (\(n, env, e, c, regEnv) -> (regEnv, (n, env, e, c, regEnv)))

setFuncArgNr            :: Int -> ST ()
setFuncArgNr i          =  S (\(n, env, e, c, regEnv) -> ((), (n, env `setExtra` i, e, c, regEnv)))

setRegExtra             :: LevelInfo -> ST ()
setRegExtra info        =  S (\(n, env, e, c, regEnv) -> ((), (n, env, e, c, regEnv `setExtra` info)))

adjustRegs              :: Extra -> ST ()
adjustRegs e            = do regEnv <- getRegEnv
                             let m = getMap regEnv
                             let list = M.toList m
                             let f (r, Inf3 n i) = case e `getVarNumber` n of
                                                        Nothing -> return ()
                                                        Just a  -> addRegInfo r (Inf3 n a)
                             mapM_ f list

addTempVar              :: Name -> Extra -> ST()
addTempVar n e          = if "@" `isPrefixOf` n
                             then do env <- getEnv
                                     case env `getVar` n of 
                                        Nothing -> do case e `getVarNumber` n of
                                                        Nothing -> return ()
                                                        Just _  -> do info <- newInfo n
                                                                      addEnvVar n info
                                        Just p  -> return ()
                             else return ()

getEnvVar               :: Name -> ST Info
getEnvVar q             = do env <- getEnv 
                             case env `getVar` q of 
                                        Nothing -> do writeError $ "could not find " ++ (show q)
                                                      return (Inf NoReg (Pos (P SB 0)) False)
                                                      --return NoInfo
                                        Just p  -> return p
                                                   

getRegMin               :: Int -> ST Reg
getRegMin i             = do regEnv <- getRegEnv
                             let m = getMap regEnv
                             let (r, Inf3 n _) = minimumBy comp (M.toList m)
                                        where
                                                comp (_, Inf3 _ x) (_, Inf3 _ y) | x > i     = Prelude.LT
                                                                                 | y > i     = Prelude.GT
                                                                                 | otherwise = compare x y
                             saveVar n
                             return r

getRegVar               :: Int -> ST (Maybe Reg)
getRegVar i             = do regEnv <- getRegEnv
                             let m = getMap regEnv
                             let f (Inf3 _ x) | x > i     = True
                                              | otherwise = False
                             case listToMaybe (M.toList $ M.filter f m) of
                                Nothing              -> return Nothing
                                Just (r, (Inf3 n _)) -> do saveVar n
                                                           return $ Just r

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
                                                Nothing -> getRegMin i
                                                Just r  -> return r
                                Just r  -> return r
getReg e                = do writeError $ "invalid extra: " ++ show e
                             return TEMP

allocateRegSpace        :: ST ()
allocateRegSpace        = do env <- getRegEnv
                             let Inf2 (Rs a gRegs) ns _ = getExtra env
                             let f r = do dis <- getEnvDisplacement
                                          addEnvDisplacement 1
                                          emit       (ADD SP SP (VAL 1))
                                          return (r, P SB dis)
                             regs <- mapM f gRegs
                             let g (r, imd) = emit       (STR r imd)
                             mapM_ g regs
                             setRegExtra (Inf2 (Rs a gRegs) ns regs)

restoreRegisters        :: ST ()
restoreRegisters        = do env <- getRegEnv
                             let Inf2 (Rs a b) ns regs = getExtra env
                             let f (r, imd) = emit (LDR r imd)
                             mapM_ f regs
                             setRegExtra (Inf2 (Rs a b) ns [])
                             
restoreRegisters'       :: ST ()
restoreRegisters'       = do env <- getRegEnv
                             let Inf2 _ _ regs = getExtra env
                             let f (r, imd) = emit (LDR r imd)
                             mapM_ f regs
                             

saveVar'                                :: Name -> Info -> ST ()
saveVar' _ (Inf NoReg _ _)              = return ()
saveVar' n (Inf (Rreg r) (Pos imd) _)   = do emit       (STR r imd)
                                             setEnvVar n (Inf NoReg (Pos imd) True)

saveVar                 :: Name -> ST ()
saveVar n               = do if "#" `isPrefixOf` n
                                then return ()
                                else do env <- getEnv
                                        info <- getEnvVar n
                                        saveVar' n info      

revertRegChanges        :: ST ()
revertRegChanges        = do E m1 _ r1 (E m2 d r2 e) <- getRegEnv
                             let list1 = M.toList m1
                             let f (r, Inf3 n1 i) = do case M.lookup r m2 of
                                                        Nothing          -> return ()
                                                        Just (Inf3 n2 _) -> if n1 == n2 || "#" `isPrefixOf` n2
                                                                                        then return ()
                                                                                        else do emit (DEBUG $ "revering reg: " ++ show r)
                                                                                                saveVar n1
                                                                                                compVal' (IVar n2) r
                                                                                                Inf _ imd saved <- getEnvVar n2
                                                                                                setEnvVar n2 (Inf (Rreg r) imd saved)
                             -- restore registers to their original value
                             remRegLevel
                             mapM_ f list1
                             let g (r, Inf3 n1 i) = do case M.lookup r m1 of
                                                        Nothing         -> do Inf _ (Pos imd) saved <- getEnvVar n1
                                                                              emit (DEBUG $ "revering var from memory: " ++ show n1)
                                                                              writeError $ "revering var from memory: " ++ show n1 ++ " ?"
                                                                              emit (LDR r imd)
                                                        Just _          -> return()
                             -- load removed registers (probably used for an integer value) from memory
                             let list2 = M.toList m2
                             mapM_ g list2
                             
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

{-correctStackAlignment   :: ST ()
correctStackAlignment   = do dis <- getTotalDisplacement
                             if dis `mod` 2 /= 0 then do addEnvDisplacement 1
                                                         emit (ADD SP SP (VAL 1))
                                                 else return ()-}
                              
removeGeneralReg        :: Reg -> ST ()
removeGeneralReg r      =  do env <- getRegEnv 
                              let Inf2 (Rs a gReg) ns b = getExtra env
                              let gReg' = delete r gReg
                              setRegExtra (Inf2 (Rs a gReg') ns b)
                          
-- name of variable, register to be assigned to, extra info
newInfoReg              :: Name -> Reg -> ST Info
newInfoReg n r          = do removeFuncReg r n
                             dis <- getEnvDisplacement
                             addEnvDisplacement 1
                             let imd = P SB dis
                             emit       (ADD SP SP (VAL 1))
                             return (Inf (Rreg r) (Pos imd) False)
                                
newInfo                 :: Name -> ST Info
newInfo n               = do dis <- getEnvDisplacement
                             addEnvDisplacement 1
                             let imd = P SB dis
                             emit       (ADD SP SP (VAL 1))
                             return (Inf NoReg (Pos imd) False)
                                
newInfoStack            :: Name -> Int -> Info
newInfoStack n i        = Inf NoReg (Pos $ P SB (-i)) True
                                                
-- [names of arguments] -> where should it start
addFuncArgs             :: [Name] -> Int -> ST ()
addFuncArgs [] _        = return ()
addFuncArgs (n:ns) i
           | i <= 3     = do info <- newInfoReg n (R $ "r" ++ show i)
                             addEnvVar n info
                             addFuncArgs ns (i + 1)
           | otherwise  = do let info = newInfoStack n (i - 3)
                             addEnvVar n info
                             addFuncArgs ns (i + 1)

compProg                        :: IProg -> ST ()
compProg (IGlobalVar n)         = do info <- newInfoReg n (G n)
                                     addEnvVar n info
                                     emit       (ADDRESS n)
compProg (IFun n ns st e)       = do emit (DEBUG $ "IFun" ++ show n)
                                     addEnvLevel
                                     addRegLevel
                                     --save SB, LR
                                     emitCode   [LABEL (N n), PUSHV SB, MOV SB (P SP 0), PUSHV LR]
                                     setEnvDisplacement 2
                                     allocateRegSpace
                                     --prepare the environment for function arguments
                                     addFuncArgs ns 0
                                     let stackArg = Prelude.length ns - 4
                                     if stackArg > 0 then setFuncArgNr stackArg
                                                     else setFuncArgNr 0
                                     saveFuncRegVars
                                     compStmt st
                                     restoreRegisters
                                     envDis <- getEnvDisplacement
                                     emitCode   [SUB SP SP (VAL (envDis - 2)), POP LR, POP SB]
                                     if stackArg > 0 then emit (SUB SP SP (VAL stackArg))
                                                     else return ()
                                     emitCode   [BX NONE LR]
                                     -- restore SP to before arguments
                                     remEnvLevel
                                     remRegLevel
                                     emit (DEBUG $ "---------------" ++ "IFun" ++ show n)
compProg (IPSeq xs)             = mapM_ compProg xs
                
                
compStmt                        :: IStmt -> ST ()
compStmt (ILocalVar n e)        = do emit (DEBUG $ "ILocalVar" ++ show n)
                                     info <- newInfo n
                                     addEnvVar n info
                                     emit (DEBUG $ "---------------" ++ "ILocalVar" ++ show n)
compStmt (IAssign n val e)      = do emit (DEBUG $ "IAssign" ++ show n ++ " " ++ show val)
                                     r1 <- compName n e
                                     compVal' val r1
                                     emit (DEBUG $ "---------------" ++ "IAssign" ++ show n ++ " " ++ show val)
compStmt (IPrint str vs e)      = do emit (DEBUG $ show (IPrint str vs Empt))
                                     --compVal' val (R "r1")
                                     prepareFunctionCall vs 1
                                     let args = Prelude.length vs
                                     emit (PRINT str args)
                                     emit (DEBUG $ "---------------" ++ show (IPrint str vs Empt))
compStmt (IRead n e)            = do addTempVar n e
                                     rd <- compName n e
                                     emit (READ rd)
compStmt (E_STMT)               = return ()
compStmt (ISeqn  xs)            = mapM_ compStmt xs
compStmt (ISeqnE xs e)          = do mark <- fresh
                                     emit (DEBUG $ show (ISeqnE [] Empt) ++ show mark)
                                     dis <- getEnvDisplacement
                                     addEnvLevel2
                                     addRegLevel2
                                     setEnvDisplacement dis
                                     mapM_ compStmt xs
                                     revertRegChanges
                                     adjustRegs e
                                     dis2 <- getEnvDisplacement
                                     remEnvLevel
                                     emit (SUB SP SP (VAL (dis2 - dis)))
                                     emit (DEBUG $ "---------------" ++ show (ISeqnE [] Empt) ++ show mark)
compStmt (IWhile es v p e1 e2)  = do emit (DEBUG $ "IWhile " ++ show v)
                                     lb <- fresh
                                     lb' <- fresh
                                     mapM_ compStmt es 
                                     jumpz v lb' e1
                                     dis <- getEnvDisplacement
                                     emit (LABEL lb)
                                     compStmt p
                                     jumpz v lb' e2
                                     dis2 <- getEnvDisplacement
                                     emit (SUB SP SP (VAL (dis2 - dis)))
                                     emitCode   [B NONE lb, LABEL lb']
                                     emit (DEBUG $ "---------------" ++ "IWhile " ++ show v)
compStmt (IIf v p1 E_STMT e)    = do emit (DEBUG $ "IIf " ++ show v)
                                     lb <- fresh
                                     jumpz v lb e
                                     compStmt p1
                                     emit (LABEL lb)
                                     emit (DEBUG $ "---------------" ++ "IIf " ++ show v)
compStmt (IIf v p1 p2 e)        = do emit (DEBUG $ "IIf " ++ show v)
                                     lb <- fresh
                                     lb' <- fresh
                                     jumpz v lb e
                                     compStmt p1 
                                     emitCode [B NONE lb', LABEL lb] 
                                     compStmt p2 
                                     emit (LABEL lb')
                                     emit (DEBUG $ "---------------" ++ "IIf " ++ show v)
compStmt (IReturn v e)          = do emit (DEBUG $ show (IReturn v Empt))
                                     compVal' v (R "r0")
                                     restoreRegisters'
                                     envDis <- getEnvDisplacement
                                     emitCode   [SUB SP SP (VAL (envDis - 2)), POP LR, POP SB]
                                     env <- getEnv
                                     let stackArg = getExtra env
                                     if stackArg > 0 then emit (SUB SP SP (VAL stackArg))
                                                     else return ()
                                     emitCode   [BX NONE LR]
                                     emit (DEBUG "---------------")
compStmt (IApply n vals res e)  = do emit (DEBUG $ "IApply " ++ show n)
                                     addTempVar res e -- result
                                     prepareFunctionCall vals 0
                                     --correctStackAlignment
                                     emit (BL NONE (N n))
                                     emit (DEBUG "---------------")
compStmt (IApp n Div v1 v2 e)   = do emit (DEBUG $ show (IApp n Div v1 v2 Empt))
                                     addTempVar n e
                                     let r0 = (R "r0")
                                     let r1 = (R "r1")
                                     compVal' v1 r0
                                     compVal' v2 r1
                                     --correctStackAlignment
                                     emit (DIV r0 r0 r1)
                                     rd <- compName n e
                                     emit (MOV rd (P r0 0))
                                     emit (DEBUG "---------------")
compStmt (IApp n Mod v1 v2 e)   = do emit (DEBUG $ show (IApp n Mod v1 v2 Empt))
                                     addTempVar n e
                                     let r0 = (R "r0")
                                     let r1 = (R "r1")
                                     compVal' v1 r0
                                     compVal' v2 r1
                                     --correctStackAlignment
                                     emit (MOD r1 r0 r1)
                                     rd <- compName n e
                                     emit (MOV rd (P r1 0))
                                     emit (DEBUG "---------------")
compStmt (IApp n op v1 v2 e)    = do emit (DEBUG $ show (IApp n op v1 v2 Empt))
                                     addTempVar n e
                                     adjustRegs e
                                     rd <- compName n e
                                     r1 <- compVal v1 e
                                     r2 <- compVal v2 e
                                     check (rd,r1,r2) (n, v1, v2)
                                     case op of Add -> emit (ADD rd r1 (P r2 0))
                                                Sub -> emit (SUB rd r1 (P r2 0))
                                                Mul -> emit (MUL rd r1 (P r2 0))
                                     emit (DEBUG "---------------")
compStmt (IBreak pos)           = emit (BREAK pos)


check                           :: (Reg, Reg, Reg) -> (Name, Value, Value) -> ST()
check (r0, r1, r2) (n, v1, v2)  = do check' (r1,r2) (v1,v2)
                                     check'' (r0,r1) (n,v1)
                                     check'' (r0,r2) (n,v2)
                        
check' (r0, r1) (v0, v1)        = if r0 /= r1 then return ()
                                              else if v0 /= v1 then do regEnv <- getRegEnv
                                                                       let m = getMap regEnv
                                                                       writeError $ show v0 ++ " and " ++ show v1 ++ " assigned1 to " ++ show r0 ++ ":" ++ show regEnv
                                                               else return ()

check'' (r0, r1) (n, v)         = if r0 /= r1 then return ()
                                              else case v of 
                                                IVar n' -> if n /= n' then do regEnv <- getRegEnv
                                                                              let m = getMap regEnv
                                                                              writeError $ show n ++ " and " ++ show n' ++ " assigned2 to " ++ show r0 ++ ":" ++ show regEnv
                                                                     else return ()
                                                _       -> do regEnv <- getRegEnv
                                                              let m = getMap regEnv
                                                              writeError $ show n ++ " and " ++ show v ++ " assigned3 to " ++ show r0 ++ ":" ++ show regEnv

compVal                         :: Value -> Extra -> ST Reg
compVal (IVal i) (I1 map ln)    = do reg <- getReg (I1 map ln)
                                     emit (MOV reg (VAL i))
                                     addRegInfo reg (Inf3 ('#':(show i)) ln)
                                     return reg
compVal (IVar n) e              = compName n e
compVal LastReturn   _          = return (R "r0")

compName                        :: Name -> Extra -> ST Reg
compName n e                    = do Inf reg (Pos imd) saved <- getEnvVar n
                                     case reg of 
                                        NoReg  -> do reg' <- getReg e
                                                     case e `getVarNumber` n of
                                                        Nothing -> addRegInfo reg' (Inf3 n 0)
                                                        Just a  -> addRegInfo reg' (Inf3 n a)
                                                     setEnvVar n (Inf (Rreg reg') (Pos imd) saved)
                                                     emit (LDR reg' imd)
                                                     return reg'
                                                     {-if saved then do emit (LDR reg' imd)
                                                                      return reg'
                                                              else return reg'-}
                                        Rreg r -> do case e `getVarNumber` n of
                                                        Nothing -> addRegInfo r (Inf3 n 0)
                                                        Just a  -> addRegInfo r (Inf3 n a)
                                                     setEnvVar n (Inf reg (Pos imd) saved)
                                                     return r

compVal'                        :: Value -> Reg -> ST ()
compVal' (IVal i) reg           = emit (MOV reg (VAL i))
compVal' (IVar n) reg           = do Inf r (Pos imd) _ <- getEnvVar n
                                     case r of
                                        NoReg  -> emit (LDR reg imd)
                                        Rreg r -> if reg /= r then emit (MOV reg (P r 0)) else return ()
compVal' LastReturn reg         = if (R "r0") /= reg then emit (MOV reg (P (R "r0") 0)) else return ()



pushVal                         :: Value -> ST ()
pushVal (IVal i)                = do emit (MOV TEMP (VAL i))
                                     emit (PUSHV TEMP)
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
                | otherwise     = do prepareFunctionCall vs (i + 1)
                                     pushVal v
                                     

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
                                     
condOpposite            :: Cond -> Cond
condOpposite AST.EQ     = NE          
condOpposite NE         = AST.EQ
condOpposite AST.GT     = LE
condOpposite AST.LT     = GE
condOpposite GE         = AST.LT
condOpposite LE         = AST.GT