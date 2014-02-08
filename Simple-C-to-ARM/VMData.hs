
module VMData (
    Stack (..),
    Mem   (..),
    Code  (..),
    Inst  (..),
    Label (..)
) where

import Interm

-- Virtual machine
-- ===============

type Stack            =  [Int]

type Mem              =  [(Name,Int)]

type Code             =  [Inst]

data Inst             =  ADDRESS Name
                      |  PUSH Int
                      |  PUSHV Name
                      |  POP Name
                      |  DO Op
                      |  JUMP Label
                      |  JUMPZ Label
                      |  LABEL Label
                      |  PRINT
                         deriving (Show, Eq)

type Label            =  Int