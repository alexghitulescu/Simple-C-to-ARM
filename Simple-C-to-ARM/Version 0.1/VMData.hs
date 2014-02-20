
module VMData (
    Stack (..),
    Mem   (..),
    Code  (..),
    Inst  (..),
    Label (..)
) where

import Interm
import Data.Map

-- Virtual machine
-- ===============

type Stack            =  [Int]

type Mem              =  [(Name,Int)]

type Code             =  [Inst]

type Reg              =  Map String Int

data Inst             =  ADDRESS Name
                      |  PUSH Int
                      |  PUSHV Name
                      |  POP Name
                      |  DO Op
                      |  JUMP Label
                      |  JUMPZ Label
                      |  LABEL Label
                      |  ADD 
                      |  PRINT
                         deriving (Show, Eq)

type Label            =  Int