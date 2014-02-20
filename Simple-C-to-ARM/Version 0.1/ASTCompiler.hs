
module ASTCompiler (
     comp
) where

import AST
import VMInst

comp                            :: Prog -> Code
comp p                          = fst $ runState (compprogM p) 0

-- State Monad 
-- ===========

-- Declaration for the state monad and a new type runState to save writing State -(a, State).

data ST a     = S { runState :: State -> (a, State) }
type State    = Int

apply         :: ST a -> State -> (a,State)
apply (S f)  = f 

instance Monad ST where
      -- return :: a -> ST a
      return x   = S (\s -> (x,s))

      -- (>>=)  :: ST a -> (a -> ST b) -> ST b
      st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')

-- The function that generates the fresh labels. It is of type ST showing that it has a hidden state. 

fresh         :: ST Int
fresh         =  S (\n -> (n, n+1))

compprogM                        :: Prog -> ST Code
compprogM (NewVar n)             = return [ADDRESS n]
compprogM (Assign v e)           = return ((compexpr e) ++ [POP v])
compprogM (Print e)              = return ((compexpr e) ++ [PRINT])
compprogM (Seqn [    ])          = return []
compprogM (Seqn (x:xs))          = do code <- compprogM x
                                      code' <- compprogM (Seqn xs)
                                      return (code ++ code') 
compprogM (While e p)            = do lb <- fresh
                                      code <- compprogM p
                                      lb' <- fresh 
                                      return ([LABEL lb] ++ (compexpr e) ++ [JUMPZ (lb')] ++ code ++ [JUMP lb, LABEL (lb')])
compprogM (If e p1 (Seqn []))    = do lb <- fresh
                                      code <- compprogM p1
                                      return ((compexpr e) ++ [JUMPZ lb] ++ code ++ [LABEL lb]) 
compprogM (If e p1 p2)           = do lb <- fresh
                                      lb' <- fresh
                                      code <- compprogM p1
                                      code' <- compprogM p2
                                      return ((compexpr e) ++ [JUMPZ lb] ++ code ++ [JUMP (lb'), LABEL lb] ++ code' ++ [LABEL (lb')])
                                      
compexpr                      :: Expr -> Code
compexpr (Val n)              = [PUSH n]
compexpr (Var v)              = [PUSHV v]
compexpr (App op e1 e2)       = (compexpr e1) ++ (compexpr e2) ++ [DO op]