
module IAST (
    IProg (..),
    IStmt (..),
    Value (..),
    Extra (..)
) where

import Text.Parsec.Pos
import Data.Map
import AST

-- Imperative language
-- ===================

-- Basic declarations for the language:

data IProg            =  IGlobalVar Name
                      |  IFun Name [Name] IStmt Extra
                      |  IPSeq [IProg]
                         deriving Show

data IStmt            =  ILocalVar Name Extra 
                      |  IAssign Name Value Extra
                      |  IIf Value IStmt IStmt Extra
                      |  IWhile [IStmt] Value IStmt Extra Extra
                      |  ISeqn [IStmt]
                      |  ISeqnE [IStmt] Extra
                      |  IPrint Value Extra
                      |  IReturn Value Extra
                      |  IApply Name [Value] Name Extra
                      |  IApp Name Op Value Value Extra
                      |  E_STMT
                         deriving Show
                         
data Value            =  IVal Int 
                      |  IVar Name                       
                      |  LastReturn
                      |  ILit Name
                      |  IComp Cond Value Value
                         deriving (Show, Ord, Eq)
                         
data Extra            =  Empt 
                      |  I1 (Map Name Int) Int
                         deriving (Show, Eq)