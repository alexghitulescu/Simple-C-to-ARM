
module VMRunner (
     exec
) where

import AST
import VMInst
import ASTCompiler
import SampleProg

exec :: Code -> Mem
exec c = mem where (_, mem, _) = (exec' c (0, [], []))

exec' :: Code -> (Int, Mem, Stack) -> (Int, Mem, Stack)
exec' c (n, m, s) = if n < (length c) then exec' c (execInst c n m s) else (n, m, s)

execInst :: Code -> Int -> Mem -> Stack -> (Int, Mem, Stack)
execInst c nr m s = case c !! nr of (PUSH n)  -> (nr + 1, m, n:s)
                                    (PUSHV n) -> (nr + 1, m, find n m : s)
                                    (POP n)   -> (nr + 1, (n, head s):(remP n m), tail s)
                                    (DO op)   -> (nr + 1, m, (compNr op (head s) (head (tail s))):(tail (tail s)))
                                    (JUMP l)  -> (elemIndex 0 (LABEL l) c, m, s)
                                    (JUMPZ l) -> (if head(s) == 0 then elemIndex 0 (LABEL l) c else nr + 1, m, tail s)
                                    (LABEL l) -> (nr + 1, m, s)
                                    (ADDRESS n)->(nr + 1, m, s)
                                    (PRINT)   -> (nr + 1, m, s)
                                    
                                    
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

elemIndex :: Eq a => Int -> a -> [a] -> Int
elemIndex n a [] = 0
elemIndex n a (x:xs) = if x == a then n else elemIndex (n + 1) a xs
