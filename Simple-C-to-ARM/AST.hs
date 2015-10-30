
module AST (
    Prog (..),
    Stmt (..),
    Expr (..),
    Name (..),
    Op   (..)
) where

import Text.Parsec.Pos

-- Imperative language
-- ===================

-- Basic declarations for the language:

data Prog             =  GlobalVar Name SourcePos
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

data Expr             =  Val SourcePos Integer 
                      |  Var SourcePos Name 
                      |  App SourcePos Op Expr Expr
                      |  Apply Name [Expr] 
                         deriving Show


type Name             =  String

data Type             =  Int | Str 
                         deriving Show

data Op               =  Add | Sub | Mul | Div
                         deriving (Show , Eq)