
module VMRunner (
     execM
) where

import Prelude hiding (EQ, LT, GT, compare)
import AST
import VMInst
import ASTCompiler
import SampleProg

execM                            :: Code -> State
execM c                          = fst $ runState (execCode c) (elemIndex 0 (LABELS "main") c, 0, toInteger(length c), [], [], NONE')

-- State Monad 
-- ===========

-- Declaration for the state monad and a new type runState to save writing State -(a, State).

data ST a     = S { runState :: State -> (a, State) }
type State    = (Integer, Integer, Integer, Mem, Stack, CFlag)

apply         :: ST a -> State -> (a,State)
apply (S f)  = f 

instance Monad ST where
      -- return :: a -> ST a
      return x   = S (\s -> (x,s))

      -- (>>=)  :: ST a -> (a -> ST b) -> ST b
      st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')

-- The function that generates the fresh labels. It is of type ST showing that it has a hidden state. 

--fresh         :: ST Integer
--fresh         =  S (\n -> (n, n+1))

next          :: ST ()
next          =  S (\(pc, sb, lr, m, s, cflag) -> ((), (pc + 1, sb, lr, m, s, cflag)))

nothing       :: ST ()
nothing       =  S (\(pc, sb, lr, m, s, cflag) -> ((), (pc, sb, lr, m, s, cflag)))

pop           :: ST Integer
pop           =  S (\(pc, sb, lr, m, h:s, cflag) -> (h, (pc, sb, lr, m, s, cflag)))

push          :: Integer -> ST ()
push i        =  S (\(pc, sb, lr, m, s, cflag) -> ((), (pc, sb, lr, m, i:s, cflag)))  

pushV         :: Name -> ST ()
pushV n       =  S (\(pc, sb, lr, m, s, cflag) -> ((), (pc, sb, lr, m, find n m : s, cflag)))

put           :: (Name, Integer) -> ST ()
put (n, i)    =  S (\(pc, sb, lr, m, s, cflag) -> ((), (pc, sb, lr, (n, i):(remP n m), s, cflag)))

retrieve      :: Code -> ST Inst
retrieve c    =  S (\(pc, sb, lr, m, s, cflag) -> (if pc < toInteger(length c) then c !! fromInteger(pc) else HALT, (pc, sb, lr, m, s, cflag)))

jump          :: Code -> Label -> ST ()
jump c l      =  S (\(pc, sb, lr, m, s, cflag) -> ((), (elemIndex 0 (LABEL l) c, sb, lr, m, s, cflag)))

jumpS         :: Code -> Name -> ST ()
jumpS c n     =  S (\(pc, sb, lr, m, s, cflag) -> ((), (elemIndex 0 (LABELS n) c, sb, lr, m, s, cflag)))

jumpI          :: Code -> Integer -> ST ()
jumpI c i      =  S (\(pc, sb, lr, m, s, cflag) -> ((), (i, sb, lr, m, s, cflag)))

mem           :: ST Mem
mem           =  S (\(pc, sb, lr, m, s, cflag) -> (m, (pc, sb, lr, m, s, cflag)))

state         :: ST State
state         =  S (\(pc, sb, lr, m, s, cflag) -> ((pc, sb, lr, m, s, cflag), (pc, sb, lr, m, s, cflag)))

savePCToLR    :: ST ()
savePCToLR    =  S (\(pc, sb, lr, m, s, cflag) -> ((), (pc, sb, pc, m, s, cflag)))

setCflag      :: CFlag -> ST ()
setCflag flag =  S (\(pc, sb, lr, m, s, cflag) -> ((), (pc, sb, lr, m, s, flag)))

validCflag    :: Cond -> ST Bool
validCflag cf =  S (\(pc, sb, lr, m, s, cflag) -> (compareCF cflag cf, (pc, sb, lr, m, s, cflag)))

getRegVal       :: Reg -> ST Integer
getRegVal SB    =  S (\(pc, sb, lr, m, s, cflag) -> (sb, (pc, sb, lr, m, s, cflag)))
getRegVal PC    =  S (\(pc, sb, lr, m, s, cflag) -> (pc, (pc, sb, lr, m, s, cflag)))
getRegVal LR    =  S (\(pc, sb, lr, m, s, cflag) -> (lr, (pc, sb, lr, m, s, cflag)))
getRegVal SP    =  S (\(pc, sb, lr, m, s, cflag) -> (toInteger(length s), (pc, sb, lr, m, s, cflag)))
getRegVal (R n) =  S (\(pc, sb, lr, m, s, cflag) -> (find n m, (pc, sb, lr, m, s, cflag)))

setRegVal         :: Reg -> Integer -> ST ()
setRegVal SB i    =  S (\(pc, sb, lr, m, s, cflag) -> ((), (pc, i, lr, m, s, cflag)))
setRegVal PC i    =  S (\(pc, sb, lr, m, s, cflag) -> ((), (i, sb, lr, m, s, cflag)))
setRegVal LR i    =  S (\(pc, sb, lr, m, s, cflag) -> ((), (pc, sb, i, m, s, cflag)))
setRegVal SP i    =  S (\(pc, sb, lr, m, s, cflag) -> ((), (pc, sb, lr, m, setStackSize i s, cflag)))
setRegVal (R n) i =  put (n, i)

load            :: Integer -> ST Integer
load i          =  S (\(pc, sb, lr, m, s, cflag) -> (getStackValue i s, (pc, sb, lr, m, s, cflag)))

store           :: Integer -> Integer -> ST ()
store pos i     =  S (\(pc, sb, lr, m, s, cflag) -> ((), (pc, sb, lr, m, setStackValue pos i s, cflag)))



execCode        :: Code -> ST State
execCode c      = do inst <- retrieve c
                     next
                     case inst of (PUSH i)        -> do push i
                                                        execCode c
                                  (PUSHV n)       -> do pushV n
                                                        execCode c
                                  (POP n)         -> do val <- pop
                                                        put (n, val)
                                                        execCode c
                                  (DO op)         -> do val1 <- pop
                                                        val2 <- pop
                                                        push (compNr op val1 val2)
                                                        execCode c
                                  (BX cond l)     -> execInstBX c cond l
                                  (BXL cond l)    -> do savePCToLR
                                                        execInstBX c cond l
                                  (B cond n)      -> execInstB c cond n
                                  (BL cond n)     -> do savePCToLR
                                                        execInstB c cond n
                                  (LABEL l)       -> execCode c
                                  (ADDRESS n)     -> do put (n, 0)
                                                        execCode c
                                  (PRINT)         -> do pop
                                                        execCode c
                                  (LABELS n)      -> execCode c
                                  (HALT)          -> do s <- state
                                                        return s
                                  (LDR r1 r2 d)   -> do r2val <- getRegVal r2
                                                        val <- load (r2val + d)
                                                        setRegVal r1 val
                                                        execCode c
                                  (LDRV r i)      -> do setRegVal r i
                                                        execCode c
                                  (STR r1 r2 d)   -> do r1val <- getRegVal r1
                                                        r2val <- getRegVal r2
                                                        store (r2val + d) r1val
                                                        execCode c
                                  (CMP r1 r2)     -> do r1val <- getRegVal r1
                                                        r2val <- getRegVal r2
                                                        setCflag (compare r1val r2val)
                                                        execCode c
                                  (CMPST)         -> do val1 <- pop
                                                        val2 <- pop
                                                        setCflag (compare val1 val2)
                                                        execCode c
                                  (PUSHR r)       -> do val <- getRegVal r
                                                        push val
                                                        execCode c
                                  (BR)            -> do val <- pop
                                                        jumpI c val
                                                        execCode c
                                                        

execInstBX              :: Code -> Cond -> Label -> ST State
execInstBX c cond l     = do b <- validCflag cond
                             if b then
                                jump c l
                             else 
                                nothing
                             execCode c
                      
execInstB               :: Code -> Cond -> Name -> ST State
execInstB c cond n      = do b <- validCflag cond
                             if b then
                                jumpS c n
                             else 
                                nothing
                             execCode c

                             
setStackSize                         :: Integer -> Stack -> Stack
setStackSize n s         
        | n >  toInteger(length (s)) = setStackSize n (0:s)
        | n <  toInteger(length (s)) = setStackSize n (tail s)
        | n == toInteger(length (s)) = s
                                                  
getStackValue                        :: Integer -> Stack -> Integer
getStackValue i s                    = s !! ((length s) - i') where i' = fromInteger i 

setStackValue                        :: Integer -> Integer -> Stack -> Stack
setStackValue pos i s                = replaceNth ((length s) - pos') i s where pos' = fromInteger pos

replaceNth                     :: Int -> a -> [a] -> [a]
replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

compare                         :: Integer -> Integer -> CFlag
compare i1 i2   | i1 == i2      = EQ'
                | i1 <  i2      = LT'
                | i1 >  i2      = GT'

-- EQ | NE | GT | LT | GE | LE

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
