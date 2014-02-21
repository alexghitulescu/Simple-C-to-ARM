
module VMInst (
    Stack  (..),
    Mem    (..),
    Code   (..),
    Inst   (..),
    Label  (..)
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
                      |  JUMP Label
                      |  JUMPZ Label
                      |  JUMPS Name
                      |  LABEL Label
                      |  LABELS Name
                      |  PRINT
                      |  POPB
                      |  HALT
                         deriving (Show, Eq)

type Label            =  Integer