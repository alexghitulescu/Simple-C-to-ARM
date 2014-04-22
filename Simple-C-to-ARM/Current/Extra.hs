module Extra (
) where

import Prelude hiding (EQ)
import AST
import VMInst
import qualified Data.Map as M


data Env a                      = EMPTY_ENV | E (M.Map Name a) Integer (Env a)
                                  deriving Show
