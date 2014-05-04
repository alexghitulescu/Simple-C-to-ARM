module Extra (
    addName,
    addValue,
    addValues,
    incNumber,
    copyNumber,
    mergeExtraL,
    mergeExtraR,
    getVarNumber
) where

import Prelude hiding (EQ)
import Data.Map as Map
import AST
import IAST

emptyExtra i = I1 empty i

addValue                                :: Extra -> Value -> Extra
addValue Empt v                         = addValue (emptyExtra 0) v
addValue (I1 map i) (IVar n)            = I1 (insert n i map) i
addValue e        (IComp _ v1 v2)       = let extra = addValue e v1 in addValue extra v2
addValue e        _                     = e

addValues                               :: Extra -> [Value] -> Extra
addValues e []                          = e
addValues e (v:vs)                      = let e' = e `addValue` v in addValues e' vs

addName                                 :: Extra -> Name -> Int -> Extra
addName Empt n nr                       = addName (emptyExtra 0) n nr
addName (I1 map i) n nr                 = I1 (insert n nr map) i

copyNumber                              :: Extra -> Extra -> Extra
copyNumber Empt (I1 map i)              = emptyExtra i
copyNumber (I1 map _) (I1 _ i)          = I1 map i 

incNumber                               :: Extra -> Int -> Extra
incNumber Empt nr                       = emptyExtra nr
incNumber (I1 map i) nr                 = I1 map (i + nr)

getVarNumber                            :: Extra -> Name -> Maybe Int
getVarNumber Empt n                     = Nothing
getVarNumber (I1 map _) n               = case Map.lookup n map of
                                                Nothing -> Nothing
                                                Just nr  -> Just nr

mergeExtraL                             :: Extra -> Extra -> Extra
mergeExtraL Empt e                      = e
mergeExtraL e Empt                      = e
mergeExtraL (I1 m1 i) (I1 m2 _)         = I1 (m1 `union` m2) i

mergeExtraR                             :: Extra -> Extra -> Extra
mergeExtraR Empt e                      = e
mergeExtraR e Empt                      = e
mergeExtraR (I1 m1 _) (I1 m2 i)         = I1 (m1 `union` m2) i