
module DAG (
    transform
) where

import Text.Parsec.Pos
import Data.Foldable (toList)
import qualified Data.Sequence as Sq
import qualified Data.Map as M
import Prelude hiding (EQ, LT, GT)
import AST
import IAST

transform                       :: Expr -> (Sq.Seq IStmt, Value)
transform e                     = (seq, val)
                                        where (val, (_, _, _, seq)) = runState' e

getEndState                     :: Expr -> State
getEndState e                   = snd $ runState' e
                                        
runState' e = runState (process e) (0, M.empty, M.empty, Sq.empty)

-- State Monad 
-- ===========

-- Declaration for the state monad and a new type runState to save writing State -(a, State).

data ST a     = S { runState :: State -> (a, State) }
type State    = (Integer, M.Map Tree Value, M.Map Value Tree, Sq.Seq IStmt)

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
fresh                   =  S (\(n, emap, nmap, seq) -> ("@temp" ++ show(n), (n+1, emap, nmap, seq)))

add                     :: Value -> Tree -> ST ()
add val t               =  S (\(n, emap, nmap, seq) -> ((), (n, M.insert t val emap, M.insert val t nmap, seq)))

add2                    :: Value -> Tree -> ST ()
add2 val t              =  S (\(n, emap, nmap, seq) -> ((), (n, emap, M.insert val t nmap, seq)))

addStmt                 :: IStmt -> ST ()
addStmt s               =  S (\(n, emap, nmap, seq) -> ((), (n, emap, nmap, seq Sq.|> s)))


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
process e                       = do val <- first e
                                     nameMap <- getNameMap
                                     second $ M.toAscList nameMap
                                     return val

first                           :: Expr -> ST Value
first (Val _ n)                 = do let val = IVal n
                                     add val (Leaf val)
                                     return val
first (Var _ v)                 = do let val = IVar v
                                     add val (Leaf val)
                                     return val
first (Lit _ n)                 = do let val = ILit n
                                     add val (Leaf val)
                                     return val
first (Compare _ c e1 e2)       = do v1 <- first e1
                                     v2 <- first e2
                                     let val = IComp c v1 v2
                                     add val (Leaf val)
                                     return val
first (App _ op e1 e2)          = do v1 <- first e1
                                     v2 <- first e2
                                     name <- insert (Node op v1 v2)
                                     return name
first (Apply _ n e)             = do names <- mapM first e
                                     name <- fresh
                                     let var = IVar name
                                     add2 var (Ap var n names)
                                     return var

second                                  :: [(Value, Tree)] -> ST()
second []                               = return ()
second ((_, Leaf _):ns)                 = second ns
second ((IVar n, Node op v1 v2):ns)     = do addStmt (IApp n op v1 v2)
                                             second ns
second ((IVar n, Ap _ f vs):ns)         = do addStmt (IApply f vs n)
                                             addStmt (IAssign n LastReturn)
                                             second ns
                                     
                                     
-- used for testing                                     
pos :: SourcePos
pos = (initialPos "a")                                     
                                     
test1 :: Expr
test1 = App pos Add (App pos Add (Var pos "A") (Var pos "B")) (App pos Add (Var pos "A") (Var pos "B"))

test2 :: Expr
test2 = App pos Add 
                (Apply pos "fun" [(App pos Add (Var pos "A") (Var pos "B"))]) 
                (App pos Add 
                        (App pos Add (Var pos "A") (Var pos "B")) 
                        (App pos Add (Var pos "A") (Var pos "B")))
                        
test3 = Apply pos "fun2" [
                (Apply pos "fun" [(App pos Add (Var pos "A") (Var pos "B"))]),
                (App pos Add 
                        (App pos Add (Var pos "A") (Var pos "B")) 
                        (App pos Add (Var pos "A") (Var pos "B")))]
                        
test4 = Compare pos GT 
                (Apply pos "fun" [(App pos Add (Var pos "A") (Var pos "B"))])
                (App pos Add 
                        (App pos Add (Var pos "A") (Var pos "B")) 
                        (App pos Add (Var pos "A") (Var pos "B")))