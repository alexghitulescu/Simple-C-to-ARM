
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

data Stmt             =  Ex Expr
                      |  LocalVar Name  
                      |  Assign Name Expr
                      |  If Expr Stmt Stmt
                      |  While Expr Stmt
                      |  Seqn [Stmt]
                      |  Print Expr
                      |  Return Expr
                         deriving Show

data Expr             =  Val Integer | Var Name | App Op Expr Expr | Apply Name [Expr]
                         deriving Show


type Name             =  String

data Type             =  Int | Str 
                         deriving Show

data Op               =  Add | Sub | Mul | Div
                         deriving (Show , Eq)