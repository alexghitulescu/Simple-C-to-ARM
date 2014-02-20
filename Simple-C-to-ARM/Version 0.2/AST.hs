
module AST (
    Prog (..),
    Expr (..),
    Name (..),
    Op   (..)
) where

-- Imperative language
-- ===================

-- Basic declarations for the language:

data Prog             =  NewVar Name  
                      |  Assign Name Expr
                      |  If Expr Prog Prog
                      |  While Expr Prog
                      |  Seqn [Prog]
                      |  Print Expr
                         deriving Show


data Expr             =  Val Integer | Var Name | App Op Expr Expr
                         deriving Show


type Name             =  String


data Op               =  Add | Sub | Mul | Div
                         deriving (Show , Eq)