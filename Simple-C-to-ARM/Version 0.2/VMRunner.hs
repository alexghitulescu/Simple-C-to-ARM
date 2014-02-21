
module VMRunner (
     execM
) where

import AST
import VMInst
import ASTCompiler
import SampleProg

execM                            :: Code -> State
execM c                          = fst $ runState (execCode c) (elemIndex 0 (LABELS "main") c, [], [1000000])

-- State Monad 
-- ===========

-- Declaration for the state monad and a new type runState to save writing State -(a, State).

data ST a     = S { runState :: State -> (a, State) }
type State    = (Integer, Mem, Stack)

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
next          =  S (\(pc, m, s) -> ((), (pc + 1, m, s)))

nothing       :: ST ()
nothing       =  S (\(pc, m, s) -> ((), (pc, m, s)))

pop           :: ST Integer
pop           =  S (\(pc, m, h:s) -> (h, (pc, m, s)))

push          :: Integer -> ST ()
push i        =  S (\(pc, m, s) -> ((), (pc, m, i:s)))  

pushV         :: Name -> ST ()
pushV n       =  S (\(pc, m, s) -> ((), (pc, m, find n m : s)))

put           :: (Name, Integer) -> ST ()
put (n, i)    =  S (\(pc, m, s) -> ((), (pc, (n, i):(remP n m), s)))

retrieve      :: Code -> ST Inst
retrieve c    =  S (\(pc, m, s) -> (if pc < toInteger(length c) then c !! fromInteger(pc) else HALT, (pc, m, s)))

jump          :: Code -> Label -> ST ()
jump c l      =  S (\(pc, m, s) -> ((), (elemIndex 0 (LABEL l) c, m, s)))

jumpS         :: Code -> Name -> ST ()
jumpS c n     =  S (\(pc, m, s) -> ((), (elemIndex 0 (LABELS n) c, m, s)))

mem           :: ST Mem
mem           =  S (\(pc, m, s) -> (m, (pc, m, s)))

state         :: ST State
state         =  S (\(pc, m, s) -> ((pc, m, s), (pc, m, s)))

execCode        :: Code -> ST State
execCode c      = do inst <- retrieve c
                     next
                     case inst of (PUSH i)    -> do push i
                                                    execCode c
                                  (PUSHV n)   -> do pushV n
                                                    execCode c
                                  (POP n)     -> do val <- pop
                                                    put (n, val)
                                                    execCode c
                                  (DO op)     -> do val1 <- pop
                                                    val2 <- pop
                                                    push (compNr op val1 val2)
                                                    execCode c
                                  (JUMP l)    -> do jump c l
                                                    execCode c
                                  (JUMPZ l)   -> do val <- pop
                                                    if val == 0 then 
                                                       jump c l
                                                    else
                                                       nothing
                                                    execCode c
                                  (LABEL l)   -> execCode c
                                  (ADDRESS n) -> do put (n, 0)
                                                    execCode c
                                  (PRINT)     -> do pop
                                                    execCode c
                                  (LABELS n)  -> execCode c
                                  (JUMPS n)   -> do jumpS c n
                                                    execCode c
                                  (POPB)      -> do addr <- pop
                                                    jump c addr
                                                    execCode c
                                  (HALT)      -> do s <- state
                                                    return s

                                    
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
