
module Optimiser (
    optmimise
) where

import Text.Parsec.Pos
import Text.Printf
import Data.Foldable (toList)
import qualified Data.Sequence as Sq
import qualified Data.Map as M
import Prelude hiding (EQ, LT, GT)
import AST
import IAST
import Helper
import Extra

optimise                        :: Expr -> Int -> (Sq.Seq IStmt, Value, Int)
optimise e n                    = (seq, val, n1)
                                        where (val, (n1, _, _, seq)) = runState' e n
                                        
runState' e n = runState (process e) (n, M.empty, M.empty, Sq.empty)

-- State Monad 
-- ===========

-- Declaration for the state monad and a new type runState to save writing State -(a, State).

data ST a     = S { runState :: State -> (a, State) }
type State    = (Int, M.Map Tree Value, M.Map Value Tree, Sq.Seq IStmt)

apply         :: ST a -> State -> (a,State)
apply (S f)  = f 

instance Monad ST where
      -- return :: a -> ST a
      return x   = S (\s -> (x,s))

      -- (>>=)  :: ST a -> (a -> ST b) -> ST b
      st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')

data Tree               =  Leaf Value 
                        |  Node Op Value Value 
                        |  Ap Value Name [Value]
                           deriving (Show, Ord, Eq)
      
-- The function that generates the fresh labels. It is of type ST showing that it has a hidden state. 

getEntryMap             :: ST (M.Map Tree Value)
getEntryMap             =  S (\(n, emap, nmap, seq) -> (emap, (n, emap, nmap, seq)))

getNameMap              :: ST (M.Map Value Tree)
getNameMap              =  S (\(n, emap, nmap, seq) -> (nmap, (n, emap, nmap, seq)))

fresh                   :: ST Name
fresh                   =  S (\(n, emap, nmap, seq) -> ("@t" ++ (printf "%07d" n), (n+1, emap, nmap, seq)))

add                     :: Value -> Tree -> ST ()
add val t               =  S (\(n, emap, nmap, seq) -> ((), (n, M.insert t val emap, M.insert val t nmap, seq)))

add2                    :: Value -> Tree -> ST ()
add2 val t              =  S (\(n, emap, nmap, seq) -> ((), (n, emap, M.insert val t nmap, seq)))

addStmt                 :: IStmt -> ST ()
addStmt s               =  S (\(n, emap, nmap, seq) -> ((), (n, emap, nmap, seq Sq.|> s)))


getValue                        :: Value -> ST Value
getValue (IVal x)               =  return $ IVal x
getValue (IComp c v1 v2)        =  do v1' <- getValue v1
                                      v2' <- getValue v2
                                      return $ IComp c v1' v2'
getValue v                      =  do map <- getNameMap
                                      case M.lookup v map of
                                        Nothing -> return v
                                        Just a  -> case a of Leaf (IVal x) -> return $ IVal x
                                                             _             -> return v
                                        
insert                  :: Tree -> ST Value
insert t                =  do map <- getEntryMap
                              case M.lookup t map of
                                        Nothing -> do name <- fresh
                                                      let var = IVar name
                                                      add var t
                                                      return var
                                        Just a  -> return a


-- functions

process                         :: Expr -> ST Value
process e                       = do val <- toDAG e
                                     nameMap <- getNameMap
                                     resolve $ M.toAscList nameMap
                                     val' <- getValue val
                                     return val'

toDAG                           :: Expr -> ST Value
toDAG (Val _ n)                 = do let val = IVal n
                                     add val (Leaf val)
                                     return val
toDAG (Var _ v)                 = do let val = IVar v
                                     add val (Leaf val)
                                     return val
toDAG (Lit _ n)                 = do let val = ILit n
                                     add val (Leaf val)
                                     return val
toDAG (Compare _ c e1 e2)       = do v1 <- toDAG e1
                                     v2 <- toDAG e2
                                     let val = IComp c v1 v2
                                     add val (Leaf val)
                                     return val
toDAG (App _ op e1 e2)          = do v1 <- toDAG e1
                                     v2 <- toDAG e2
                                     name <- insert (Node op v1 v2)
                                     return name
toDAG (Apply _ n e)             = do names <- mapM toDAG e
                                     name <- fresh
                                     let var = IVar name
                                     add2 var (Ap var n names)
                                     return var

resolve                                         :: [(Value, Tree)] -> ST()
resolve []                                      = return ()
resolve ((IVar n, Leaf (IComp c v1 v2)):ns)     = do v1' <- getValue v1
                                                     v2' <- getValue v2
                                                     add2 (IVar n) (Leaf (IComp c v1 v2))
resolve ((_     , Leaf _):ns)                   = resolve ns
resolve ((IVar n, Node op v1 v2):ns)            = do v1' <- getValue v1
                                                     v2' <- getValue v2
                                                     if isVal2 v1' v2' then add2 (IVar n) (Leaf (eval op v1' v2'))
                                                                       else addStmt (IApp n op v1' v2' Empt)
                                                     resolve ns
                                             
resolve ((IVar n, Ap _ f vs):ns)                = do vs' <- mapM getValue vs
                                                     addStmt (IApply f vs' n Empt)
                                                     addStmt (IAssign n LastReturn Empt)
                                                     resolve ns

eval                            :: Op -> Value -> Value -> Value
eval Add (IVal m) (IVal n)      = IVal $ m + n
eval Sub (IVal m) (IVal n)      = IVal $ m - n
eval Mul (IVal m) (IVal n)      = IVal $ m * n
eval Div (IVal m) (IVal n)      = IVal $ m `div` n