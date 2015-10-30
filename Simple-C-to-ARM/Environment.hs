module Environment (
    Env (..),
    addVar,
    getVar,
    addLevel,
    removeLevel,
    emptyTop,
    displacement,
    setDisplacement,
    addDisplacement
) where

import Prelude hiding (EQ)
import AST
import VMInst
import qualified Data.Map as M


data Env a                      = EMPTY_ENV | E (M.Map Name a) Integer (Env a)
                                  deriving Show
                                  
emptyTop                        = E M.empty 0 EMPTY_ENV

displacement                    :: Env a -> Integer
displacement (E _ i _)          = i

setDisplacement                 :: Env a -> Integer -> Env a
setDisplacement (E map _ e) i   = E map i e

addDisplacement                 :: Env a -> Integer -> Env a
addDisplacement (E map d e) i   = E map (d + i) e
                
addVar                          :: Env a -> (Name, a) -> Env a
--addVar  EMPTY_ENV  (n,p,i)      = E (M.singleton n p) i EMPTY_ENV
addVar (E map i e) (n,p)        = E (M.insert n p map) i e

getVar                          :: Env a -> Name -> Maybe a
getVar  EMPTY_ENV  n            = Nothing
getVar (E map _ e) n            = case M.lookup n map of
                                        Nothing -> getVar e n
                                        Just p  -> Just p
                                
addLevel                        :: Env a -> Env a
addLevel e                      = E M.empty 0 e

removeLevel                     :: Env a -> Env a
--removeLevel  EMPTY_ENV          = EMPTY_ENV
removeLevel (E map _ e)         = e
