
module VMInst (
    Stack  (..),
    Mem    (..),
    Code   (..),
    Inst   (..),
    Label  (..),
    Reg    (..),
    Cond   (..),
    CFlag  (..),
    Pos    (..)
) where

import AST
import Data.Array.IArray

-- Virtual machine
-- ===============

type Stack            =  Array Integer

type Mem              =  [(Name, Integer)]

type Code             =  [Inst]

data Inst             =  ADDRESS Name
                      |  PUSH Integer
                      |  PUSHV Reg
                      |  POP Reg
                      |  DO Op
                      |  CMP Reg Reg
                      |  CMPV Reg Integer
                      |  BX Cond Reg
                      |  BXL Cond Reg
                      |  B Cond Label
                      |  BL Cond Label
                      |  LABEL Label
                      |  PRINT
                      |  HALT
                      |  LDR Reg Pos
                      |  LDRV Reg Integer
                      |  STR Reg Pos
                      |  CMPST
                         deriving (Show, Eq)

data Reg              = SB | PC | LR | SP | R Name deriving (Show, Eq)

data Pos              = P Reg Displacement deriving (Show, Eq)

data Cond             = EQ | NE | GT | LT | GE | LE | NONE deriving (Show, Eq)

data CFlag            = EQ' | GT' | LT' | NONE' deriving (Show, Eq)
                         
data Label            = V Integer | N Name deriving (Show, Eq)

type Displacement     =  Integer