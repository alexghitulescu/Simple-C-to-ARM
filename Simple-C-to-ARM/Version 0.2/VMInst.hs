
module VMInst (
    Stack  (..),
    Mem    (..),
    Code   (..),
    Inst   (..),
    Label  (..),
    Reg    (..),
    Cond   (..),
    CFlag  (..)
) where

import AST

-- Virtual machine
-- ===============

type Stack            =  [Integer]

type Mem              =  [(Name, Integer)]

type Code             =  [Inst]

data Inst             =  ADDRESS Name
                      |  PUSH Integer
                      |  PUSHV Name
                      |  POP Name
                      |  DO Op
                      |  CMP Reg Reg
                      |  CMPV Reg Integer
                      |  BX Cond Label
                      |  BXL Cond Label
                      |  B Cond Name
                      |  BL Cond Name
                      |  LABEL Label
                      |  LABELS Name
                      |  PRINT
                      |  HALT
                      |  LDR Reg Reg Integer
                      |  LDRV Reg Integer
                      |  CMPST
                      |  PUSHR Reg
                      |  BR
                         deriving (Show, Eq)

data Reg              = SB | PC | LR | SP | R Name deriving (Show, Eq)

data Cond             = EQ | NE | GT | LT | GE | LE | NONE deriving (Show, Eq)

data CFlag            = EQ' | GT' | LT' | NONE' deriving (Show, Eq)
                         
type Label            =  Integer