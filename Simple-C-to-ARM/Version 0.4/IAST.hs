
module IAST (
    IProg (..),
    IStmt (..),
    Value (..)
) where

import Text.Parsec.Pos
import AST

-- Imperative language
-- ===================

-- Basic declarations for the language:

data IProg            =  IGlobalVar Name
                      |  IFun Name [Name] IStmt 
                      |  IPSeq [IProg]
                         deriving Show

data IStmt            =  ILocalVar Name 
                      |  IAssign Name Value 
                      |  IIf Value IStmt IStmt 
                      |  IWhile [IStmt] Value IStmt 
                      |  ISeqn [IStmt]
                      |  IPrint Value 
                      |  IReturn Value 
                      |  IApply Name [Value] Name
                      |  IApp Name Op Value Value 
                         deriving Show
                         
data Value            =  IVal Integer 
                      |  IVar Name                       
                      |  LastReturn
                      |  ILit Name
                      |  IComp Cond Value Value
                         deriving (Show, Ord, Eq)