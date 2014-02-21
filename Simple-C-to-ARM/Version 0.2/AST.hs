
module AST (
    Prog (..),
    Stmt (..),
    Expr (..),
    Name (..),
    Op   (..)
) where

-- Imperative language
-- ===================

-- Basic declarations for the language:

data Prog             =  GlobalVar Name
                      |  Fun Name [Name] Stmt
                      |  PSeq [Prog]
                         deriving Show

data Stmt             =  LocalVar Name  
                      |  Assign Name Expr
                      |  If Expr Stmt Stmt
                      |  While Expr Stmt
                      |  Seqn [Stmt]
                      |  Print Expr
                      |  Apply Name [Expr]
                         deriving Show


data Expr             =  Val Integer | Var Name | App Op Expr Expr
                         deriving Show


type Name             =  String


data Op               =  Add | Sub | Mul | Div
                         deriving (Show , Eq)