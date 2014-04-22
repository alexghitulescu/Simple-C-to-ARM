
module VMRunner (
     execM,
     execPrint
) where

import Prelude hiding (EQ, LT, GT, compare)
import Data.Array.IArray
import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.Sequence as Sq
import AST
import VMInst
import ASTCompiler
--import SampleProg

execM                           :: Code -> State
execM c                         = fst $ runState (start c) (elemIndex 0 (LABEL (N "main")) c, 0, toInteger(length c), 0, [], stack, NONE', Sq.empty)
                                                                                        where stack = array (0, 1000) [(i, 0) | i <- [0..1000]]

execPrint                       :: Code -> String
execPrint c                     = "(pc: " ++ show(pc) ++ ", sb: " ++ show(sb) ++ ", lr: " ++ show(lr) ++ ", sp: " ++ show(sp) ++
                                  ", mem: " ++ show(m) ++ ", stack: " ++ show(stack) ++ ", flag: " ++ show(cflag) ++ "\n stdout:\n" ++
                                  intercalate "" (toList stdout)
                                        where   stack = take (fromInteger sp) (assocs s)
                                                (pc, sb, lr, sp, m, s, cflag, stdout) = execM c
-- State Monad 
-- ===========

-- Declaration for the state monad and a new type runState to save writing State -(a, State).

data ST a     = S { runState :: State -> (a, State) }
type State    = (Integer, Integer, Integer, Integer, Mem, Stack Integer, CFlag, Sq.Seq String)

apply         :: ST a -> State -> (a,State)
apply (S f)  = f 

instance Monad ST where
      -- return :: a -> ST a
      return x   = S (\s -> (x,s))

      -- (>>=)  :: ST a -> (a -> ST b) -> ST b
      st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')



next          :: ST ()
next          =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> ((), (pc + 1, sb, lr, sp, m, s, cflag, stdout)))

nothing       :: ST ()
nothing       =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> ((), (pc, sb, lr, sp, m, s, cflag, stdout)))

pop           :: ST Integer
pop           =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> if sp < 0 then (s ! 0, (pc, sb, lr, 0, ("pop", sp):m, s, cflag, stdout))
                                                                        else (s ! sp, (pc, sb, lr, sp - 1, m, s, cflag, stdout)))

push          :: Integer -> ST ()
push i        =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> if sp < 0 then ((), (pc, sb, lr, sp + 1, ("push", sp):m, s // [(0, i)], cflag, stdout))
                                                                        else ((), (pc, sb, lr, sp + 1, m, s // [(sp + 1, i)], cflag, stdout)))  

--pushV         :: Name -> ST ()
--pushV n       =  S (\(pc, sb, lr, sp, m, s, cflag) -> ((), (pc, sb, lr, sp + 1, m, s // [(sp + 1, find n m)], cflag)))

put           :: (Name, Integer) -> ST ()
put (n, i)    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> ((), (pc, sb, lr, sp, (n, i):(remP n m), s, cflag, stdout)))

retrieve      :: Code -> ST Inst
retrieve c    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> (if pc < toInteger(length c) then c !! fromInteger(pc) else HALT, (pc, sb, lr, sp, m, s, cflag, stdout)))

jump          :: Code -> Label -> ST ()
jump c l      =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> ((), (elemIndex 0 (LABEL l) c, sb, lr, sp, m, s, cflag, stdout)))

jumpI          :: Code -> Integer -> ST ()
jumpI c i      =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> ((), (i, sb, lr, sp, m, s, cflag, stdout)))

mem           :: ST Mem
mem           =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> (m, (pc, sb, lr, sp, m, s, cflag, stdout)))

state         :: ST State
state         =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> ((pc, sb, lr, sp, m, s, cflag, stdout), (pc, sb, lr, sp, m, s, cflag, stdout)))

savePCToLR    :: ST ()
savePCToLR    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> ((), (pc, sb, pc, sp, m, s, cflag, stdout)))

setCflag      :: CFlag -> ST ()
setCflag flag =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> ((), (pc, sb, lr, sp, m, s, flag, stdout)))

validCflag    :: Cond -> ST Bool
validCflag cf =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> (compareCF cflag cf, (pc, sb, lr, sp, m, s, cflag, stdout)))

getRegVal       :: Reg -> ST Integer
getRegVal PC    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> (pc, (pc, sb, lr, sp, m, s, cflag, stdout)))
getRegVal SB    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> (sb, (pc, sb, lr, sp, m, s, cflag, stdout)))
getRegVal LR    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> (lr, (pc, sb, lr, sp, m, s, cflag, stdout)))
getRegVal SP    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> (sp, (pc, sb, lr, sp, m, s, cflag, stdout)))
getRegVal TEMP  =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> (find "scratch" m, (pc, sb, lr, sp, m, s, cflag, stdout)))
getRegVal (R n) =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> (find n m, (pc, sb, lr, sp, m, s, cflag, stdout)))
getRegVal (G n) =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> (find n m, (pc, sb, lr, sp, m, s, cflag, stdout)))

setRegVal         :: Reg -> Integer -> ST ()
setRegVal PC i    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> ((), (i, sb, lr, sp, m, s, cflag, stdout)))
setRegVal SB i    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> ((), (pc, i, lr, sp, m, s, cflag, stdout)))
setRegVal LR i    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> ((), (pc, sb, i, sp, m, s, cflag, stdout)))
setRegVal SP i    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> ((), (pc, sb, lr, i, m, s, cflag, stdout)))
setRegVal TEMP i  =  put ("scratch", i)
setRegVal (R n) i =  put (n, i)
setRegVal (G n) i =  put (n, i)

load            :: Integer -> ST Integer
load i          =  if i < 0 then S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> (s ! 0, (pc, sb, lr, sp, ("load", i):m, s, cflag, stdout)))
                            else S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> (s ! i, (pc, sb, lr, sp, m, s, cflag, stdout)))
--load i          =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> (s ! i, (pc, sb, lr, sp, m, s, cflag, stdout)))

store           :: Integer -> Integer -> ST ()
store pos i     =  if i < 0 then S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> ((), (pc, sb, lr, sp, ("store", i):m, s // [(pos, i)], cflag, stdout)))
                            else S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> ((), (pc, sb, lr, sp, m, s // [(pos, i)], cflag, stdout)))
--store pos i     =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> ((), (pc, sb, lr, sp, m, s // [(pos, i)], cflag, stdout)))

start           :: Code -> ST State
start c         = do addAddress (filter (isAddress) c)
                     execCode c
                     
printToStd        :: String -> ST ()
printToStd string = S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> ((), (pc, sb, lr, sp, m, s, cflag, stdout Sq.|> string)))

execCode        :: Code -> ST State
execCode c      = do inst <- retrieve c
                     next
                     case inst of (PUSH i)                -> do push i
                                                                execCode c
                                                                
                                  (PUSHV r)               -> do val <- getRegVal r
                                                                push val
                                                                execCode c
                                                                
                                  (POP r)                 -> do val <- pop
                                                                setRegVal r val
                                                                execCode c
                                                                
                                  (DO op)                 -> do val1 <- pop
                                                                val2 <- pop
                                                                push (compNr op val1 val2)
                                                                execCode c
                                                                
                                  (ADD rf r1 imd)         -> do val1 <- getRegVal r1
                                                                val2 <- getImdVal imd
                                                                setRegVal rf (val1 + val2)
                                                                execCode c
                                   
                                  (SUB rf r1 imd)         -> do val1 <- getRegVal r1
                                                                val2 <- getImdVal imd
                                                                setRegVal rf (val1 - val2)
                                                                execCode c
                                                                
                                  (MUL rf r1 imd)         -> do val1 <- getRegVal r1
                                                                val2 <- getImdVal imd
                                                                setRegVal rf (val1 * val2)
                                                                execCode c
                                                                
                                  (DIV rf r1 imd)         -> do val1 <- getRegVal r1
                                                                val2 <- getImdVal imd
                                                                setRegVal rf (val1 `div` val2)
                                                                execCode c
                                   
                                  (MOV r imd)             -> do val <- getImdVal imd
                                                                setRegVal r val
                                                                execCode c
                                   
                                  (BX cond reg)           -> execInstBX c cond reg
                                  (BXL cond reg)          -> do savePCToLR
                                                                execInstBX c cond reg
                                                                
                                  (B cond l)              -> execInstB c cond l
                                  (BL cond l)             -> do savePCToLR
                                                                execInstB c cond l
                                                                
                                  (LABEL l)               -> execCode c
                                                                
                                  (PRINT reg)             -> do s <- getRegVal reg
                                                                printToStd (show s ++ "\n")
                                                                execCode c
                                                                
                                  (HALT)                  -> do s <- state
                                                                return s
                                                                
                                  (LDR r1 (P r2 d))       -> do r2val <- getRegVal r2
                                                                case r2 of
                                                                        G n -> do setRegVal r1 r2val
                                                                                  execCode c
                                                                        _   -> do val <- load (r2val + d)
                                                                                  setRegVal r1 val
                                                                                  execCode c
                                                                
                                  (LDRV r i)              -> do setRegVal r i
                                                                execCode c
                                                                
                                  (STR r1 (P r2 d))       -> do r1val <- getRegVal r1
                                                                case r2 of
                                                                        G n -> do setRegVal r2 r1val 
                                                                                  execCode c
                                                                        _   -> do r2val <- getRegVal r2
                                                                                  store (r2val + d) r1val
                                                                                  execCode c
                                                                
                                  (CMP r1 imd)            -> do r1val <- getRegVal r1
                                                                imdval <- getImdVal imd
                                                                setCflag (compare r1val imdval)
                                                                execCode c
                                                                
                                  (CMPST)                 -> do val1 <- pop
                                                                val2 <- pop
                                                                setCflag (compare val1 val2)
                                                                execCode c
                                  (ADDRESS n)             -> do put ("????ADDRESS: " ++ show n, 0)
                                                                setRegVal PC 100000
                                                                execCode c
                                  (other)                 -> do put ("other: " ++ show other, 0)
                                                                setRegVal PC 100000
                                                                execCode c
                                  



addAddress                      :: Code -> ST ()
addAddress []                   = return ()
addAddress (ADDRESS n:xs)       = do put (n, 0)
                                     addAddress xs

getImdVal               :: Imd -> ST Integer
getImdVal (VAL i)       = return i
getImdVal (P r i)       = do val <- getRegVal r
                             return (val + i)
                                                        
execInstBX              :: Code -> Cond -> Reg -> ST State
execInstBX c cond reg   = do b <- validCflag cond
                             if b then
                                do pos <- getRegVal reg
                                   jumpI c pos
                             else 
                                nothing
                             execCode c
                      
execInstB               :: Code -> Cond -> Label -> ST State
execInstB c cond l      = do b <- validCflag cond
                             if b then
                                jump c l
                             else 
                                nothing
                             execCode c

compare                         :: Integer -> Integer -> CFlag
compare i1 i2   | i1 == i2      = EQ'
                | i1 <  i2      = LT'
                | i1 >  i2      = GT'

compareCF                :: CFlag -> Cond -> Bool
compareCF _ NONE         = True
compareCF NONE' _        = False
compareCF EQ' EQ         = True
compareCF EQ' GE         = True
compareCF EQ' LE         = True
compareCF EQ' _          = False
compareCF GT' GT         = True
compareCF GT' GE         = True
compareCF GT' NE         = True
compareCF GT' _          = False
compareCF LT' LT         = True
compareCF LT' LE         = True
compareCF LT' NE         = True
compareCF LT' _          = False
          
find          :: Eq a => a -> [(a,b)] -> b
find n (x:xs) = if n == (fst x) then (snd x) else find n xs 

remP           :: Eq a => a -> [(a,b)] -> [(a,b)]
remP _ [    ]  = []
remP n (x:xs)  = if (fst x) == n then xs else x : (remP n xs)

compNr          :: Op -> Integer -> Integer -> Integer
compNr Add m n  = m + n
compNr Sub m n  = n - m
compNr Mul m n  = m * n
compNr Div m n  = m `quot` n

elemIndex :: Eq a => Integer -> a -> [a] -> Integer
elemIndex n a [] = 1000000
elemIndex n a (x:xs) = if x == a then n else elemIndex (n + 1) a xs

isAddress               :: Inst -> Bool
isAddress (ADDRESS _)   = True
isAddress _             = False
