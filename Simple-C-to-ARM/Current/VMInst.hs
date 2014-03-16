
module VMInst (
    Stack  (..),
    Mem    (..),
    Code   (..),
    Inst   (..),
    Label  (..),
    Reg    (..),
    Cond   (..),
    CFlag  (..),
    Imd    (..)
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
                      |  ADD Reg Reg Imd
                      |  SUB Reg Reg Imd
                      |  MUL Reg Reg Imd
                      |  DIV Reg Reg Imd
                      |  MOV Reg Imd
                      |  CMP Reg Imd
                      |  BX Cond Reg
                      |  BXL Cond Reg
                      |  B Cond Label
                      |  BL Cond Label
                      |  LABEL Label
                      |  PRINT
                      |  HALT
                      |  LDR Reg Imd
                      |  LDRV Reg Integer
                      |  STR Reg Imd
                      |  CMPST
                         deriving (Show, Eq)

data Reg              = SB | PC | LR | SP | R Name | G Name deriving (Show, Eq)

data Imd              = P Reg Integer | VAL Integer deriving (Show, Eq)

data Cond             = EQ | NE | GT | LT | GE | LE | NONE deriving (Show, Eq)

data CFlag            = EQ' | GT' | LT' | NONE' deriving (Show, Eq)
                         
data Label            = V Integer | N Name deriving (Show, Eq)

type Displacement     =  Integer