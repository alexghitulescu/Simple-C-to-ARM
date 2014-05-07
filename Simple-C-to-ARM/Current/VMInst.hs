
module VMInst (
    Stack       (..),
    Mem         (..),
    Code        (..),
    Inst        (..),
    Label       (..),
    Reg         (..),
    CFlag       (..),
    Imd         (..),
    Registers   (..),
    registers,
    funcRegisters,
    generalRegisters
) where

import AST
import Data.Array.IArray
import Data.Map

-- Virtual machine
-- ===============

type Stack            =  Array Integer

type Code             =  [Inst]

type Mem              =  Map Name Integer

data Inst             =  ADDRESS Name
                      |  PUSHV Reg
                      |  POP Reg
                      |  ADD Reg Reg Imd
                      |  SUB Reg Reg Imd
                      |  MUL Reg Reg Imd
                      |  DIV Reg Reg Reg
                      |  MOD Reg Reg Reg
                      |  MOV Reg Imd
                      |  CMP Reg Imd
                      |  BX Cond Reg
                      |  B Cond Label
                      |  BL Cond Label
                      |  LABEL Label
                      |  PRINT
                      |  HALT
                      |  LDR Reg Imd
                      |  LDRV Reg Integer
                      |  STR Reg Imd
                      |  DEBUG String
                         deriving (Show, Eq)

data Reg              = SB | PC | LR | SP | TEMP | R Name | G Name deriving (Show, Eq, Ord)

data Registers = Rs [Reg] [Reg] deriving Show

registers = Rs funcRegisters generalRegisters

funcRegisters = [R "r0", R "r1", R "r2", R "r3"]

generalRegisters = [R "r4", R "r5", R "r6", R "r7", R "r8", R "r10", R "r11"]
--generalRegisters = [R "r4", R "r5", R "r6"]
--generalRegisters = [R "r4", R "r5", R "r6", R "r7", R "r8", R "r10", R "r11", R "r13", R "r14", R "r15", R "r16", R "r17", R "r18", R "r19", R "r20"]

data Imd              = P Reg Integer | VAL Integer deriving (Show, Eq)

data CFlag            = EQ' | GT' | LT' | NONE' deriving (Show, Eq)
                         
data Label            = V Integer | N Name deriving (Show, Eq)