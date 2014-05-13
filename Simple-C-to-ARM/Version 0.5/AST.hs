
module AST (
    Prog (..),
    Stmt (..),
    Expr (..),
    Name (..),
    Cond (..),
    Type (..),
    Op   (..)
) where

import Text.Parsec.Pos

-- Imperative language
-- ===================

-- Basic declarations for the language:

data Prog             =  GlobalVar Name SourcePos
                      |  Fun Name [Name] Stmt SourcePos
                      |  PSeq [Prog]
                         deriving Show

data Stmt             =  Ex Expr 
                      |  LocalVar Name SourcePos
                      |  Assign SourcePos Name Expr 
                      |  If Expr Stmt Stmt 
                      |  While Expr Stmt 
                      |  Seqn [Stmt]
                      |  SeqnE [Stmt]
                      |  Print Expr 
                      |  Return Expr 
                         deriving Show

data Expr             =  Val SourcePos Int 
                      |  Var SourcePos Name 
                      |  Lit SourcePos Name
                      |  Compare SourcePos Cond Expr Expr
                      |  App SourcePos Op Expr Expr
                      |  Apply SourcePos Name [Expr]
                         deriving Show

data Cond             =  EQ | NE | GT | LT | GE | LE | NONE deriving (Show, Eq, Ord)

type Name             =  String

data Type             =  Int | Str | Bool | InvalidType
                         deriving (Show, Eq)

data Op               =  Add | Sub | Mul | Div | Mod
                         deriving (Show , Eq, Ord)