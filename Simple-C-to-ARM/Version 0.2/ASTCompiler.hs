
module ASTCompiler (
     comp
) where

import Prelude hiding (EQ)
import AST
import VMInst

comp                            :: Prog -> Code
comp p                          = fst $ runState (compProg p) 0

-- State Monad 
-- ===========

-- Declaration for the state monad and a new type runState to save writing State -(a, State).

data ST a     = S { runState :: State -> (a, State) }
type State    = Integer

apply         :: ST a -> State -> (a,State)
apply (S f)  = f 

instance Monad ST where
      -- return :: a -> ST a
      return x   = S (\s -> (x,s))

      -- (>>=)  :: ST a -> (a -> ST b) -> ST b
      st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')

-- The function that generates the fresh labels. It is of type ST showing that it has a hidden state. 

fresh         :: ST Integer
fresh         =  S (\n -> (n, n+1))

newVars        :: [Name] -> Code
newVars []     = []
newVars (n:ns) = (ADDRESS n):(newVars ns)

compProg                        :: Prog -> ST Code
compProg (GlobalVar n)          = return [ADDRESS n]
compProg (Fun n ns st)          = do code <- (compStmt st)
                                     return ([LABELS n, PUSHR LR] ++ code ++ [BR])
compProg (PSeq [    ])          = return []
compProg (PSeq (x:xs))          = do code <- compProg x
                                     code' <- compProg (PSeq xs)
                                     return (code ++ code')
                
compStmt                        :: Stmt -> ST Code
compStmt (LocalVar n)             = return [ADDRESS n]
compStmt (Assign v e)           = return ((compExpr e) ++ [POP v])
compStmt (Print e)              = return ((compExpr e) ++ [PRINT])
compStmt (Seqn [    ])          = return []
compStmt (Seqn (x:xs))          = do code <- compStmt x
                                     code' <- compStmt (Seqn xs)
                                     return (code ++ code')
compStmt (While e p)            = do lb <- fresh
                                     code <- compStmt p
                                     lb' <- fresh
                                     return ([LABEL lb] ++ (compExpr e) ++ (jumpz lb') ++ code ++ [BX NONE lb, LABEL (lb')])
compStmt (If e p1 (Seqn []))    = do lb <- fresh
                                     code <- compStmt p1
                                     return ((compExpr e) ++ (jumpz lb) ++ code ++ [LABEL lb])
compStmt (If e p1 p2)           = do lb <- fresh
                                     lb' <- fresh
                                     code <- compStmt p1
                                     code' <- compStmt p2
                                     return ((compExpr e) ++ (jumpz lb) ++ code ++ [BX NONE lb', LABEL lb] ++ code' ++ [LABEL (lb')])
compStmt (Apply n e)            = return ((compExprs e) ++ [BL NONE n])

jumpz                           :: Label -> Code
jumpz lb                        = [PUSH 0, CMPST, BX EQ lb] 
                                     
compExprs                     :: [Expr] -> Code
compExprs []                  = []
compExprs (x:xs)              = (compExpr x) ++ (compExprs xs)
                                      
compExpr                      :: Expr -> Code
compExpr (Val n)              = [PUSH n]
compExpr (Var v)              = [PUSHV v]
compExpr (App op e1 e2)       = (compExpr e1) ++ (compExpr e2) ++ [DO op]