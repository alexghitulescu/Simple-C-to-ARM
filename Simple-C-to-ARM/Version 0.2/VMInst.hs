
module VMInst (
    Stack  (..),
    Mem    (..),
    Code   (..),
    Inst   (..),
    Label  (..),
    Reg    (..),
    Cond   (..)
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
                      |  CMP Reg Reg
                         deriving (Show, Eq)

data Reg              = SB | PC | LR | SP | R Name

data Cond             = EQ | NE | GT | LT | GE | LE | NONE
                         
type Label            =  Integer