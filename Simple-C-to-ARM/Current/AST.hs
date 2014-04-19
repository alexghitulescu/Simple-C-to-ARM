
module AST (
    Prog (..),
    Stmt (..),
    Expr (..),
    Name (..),
    Op   (..),
    Pos  (..)
) where

import Text.Parsec.Pos

-- Imperative language
-- ===================

-- Basic declarations for the language:

data Prog             =  GlobalVar Name Pos
                      |  Fun Name [Name] Stmt Pos
                      |  PSeq [Prog]
                         deriving Show

data Stmt             =  Ex Expr Pos
                      |  LocalVar Name Pos
                      |  Assign Name Expr Pos
                      |  If Expr Stmt Stmt Pos
                      |  While Expr Stmt Pos
                      |  Seqn [Stmt] Pos
                      |  Print Expr Pos
                      |  Return Expr Pos
                         deriving Show

data Expr             =  Val Integer Pos 
                      |  Var Name Pos 
                      |  App Op Expr Expr
                      |  Apply Name [Expr] Pos
                         deriving Show


type Name             =  String

data Type             =  Int | Str 
                         deriving Show

data Op               =  Add | Sub | Mul | Div
                         deriving (Show , Eq)
                         
data Pos              =  SourcePos
                         deriving Show