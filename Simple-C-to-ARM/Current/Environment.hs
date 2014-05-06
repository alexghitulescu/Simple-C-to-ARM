module Environment (
    Env (..),
    addVar,
    getVar,
    setVar,
    getMap,
    getExtra,
    setExtra,
    copyExtra,
    remVarCLevel,
    getVarCLevel,
    addLevelCopy,
    addLevel,
    removeLevel,
    emptyTop,
    displacement,
    setDisplacement,
    addDisplacement,
    totalDisplacement
) where

import AST
import VMInst
import qualified Data.Map as M

data Env n a r                    = EMPTY_ENV | E (M.Map n a) Integer r (Env n a r)
                                  deriving Show
                                  
emptyTop r                      = E M.empty 0 r EMPTY_ENV

displacement                    :: Env n a r -> Integer
displacement (E _ d _ _)        = d

totalDisplacement               :: Env n a r -> Integer
totalDisplacement  EMPTY_ENV    = 0
totalDisplacement (E _ d _ e)   = d + totalDisplacement e

setDisplacement                 :: Env n a r -> Integer -> Env n a r
setDisplacement (E map _ r e) i = E map i r e

addDisplacement                 :: Env n a r -> Integer -> Env n a r
addDisplacement (E map d r e) i = E map (d + i) r e
                
addVar                          :: Ord n => Env n a r -> (n, a) -> Env n a r
--addVar  EMPTY_ENV  (n,p,i)      = E (M.singleton n p) i EMPTY_ENV
addVar (E map d r e) (n,p)      = E (M.insert n p map) d r e

getVar                          :: Ord n => Env n a r -> n -> Maybe a
getVar  EMPTY_ENV  n            = Nothing
getVar (E map _ _ e) n          = case M.lookup n map of
                                        Nothing -> getVar e n
                                        Just p  -> Just p

setVar                          :: Ord n => Env n a r -> (n, a) -> Env n a r
setVar  EMPTY_ENV  (_,_)        = EMPTY_ENV
setVar (E map d r e) (n,a)      = case M.lookup n map of
                                        Nothing -> E map d r (setVar e (n,a))
                                        Just _  -> E (M.insert n a map) d r e

remVarCLevel                    ::Ord n => Env n a r -> n -> Env n a r
remVarCLevel  EMPTY_ENV  _      = EMPTY_ENV
remVarCLevel (E map d r e) n    = E (M.delete n map) d r e
                                        
getExtra                        :: Env n a r -> r
getExtra (E _ _ r _)            = r

setExtra                        :: Env n a r -> r -> Env n a r
setExtra (E m d _ e) r          = E m d r e

getMap                          :: Env n a r -> M.Map n a
getMap EMPTY_ENV                = M.empty
getMap (E map _ _ _)            = map

copyExtra                       :: Env n a r -> Env n a r -> Env n a r
copyExtra e EMPTY_ENV           = e
copyExtra EMPTY_ENV e           = let (E _ d r _) = e in E M.empty d r e
copyExtra (E map _ _ e) o       = let (E _ d r _) = o in E map d r e

getVarCLevel                    :: Ord n => Env n a r -> n -> Maybe a
getVarCLevel EMPTY_ENV  n       = Nothing
getVarCLevel (E map _ _ e) n    = M.lookup n map
                                        
addLevel                        :: Env n a r -> r -> Env n a r
addLevel e r                    = E M.empty 0 r e

addLevelCopy                    :: Env n a r -> Env n a r
addLevelCopy (E map d r e)      = E map 0 r (E map d r e)

removeLevel                     :: Env n a r -> Env n a r
--removeLevel  EMPTY_ENV          = EMPTY_ENV
removeLevel (E map _ _ e)       = e