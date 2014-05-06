module Extra (
    addName,
    addValue,
    addValue2,
    addValues,
    incNumber,
    addValues2,
    copyNumber,
    mergeExtraL,
    mergeExtraR,
    mergeExtraM,
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

addValue2                               :: Extra -> Extra -> Value -> Extra
addValue2 Empt e v                      = addValue2 (emptyExtra 0) e v
addValue2 e Empt v                      = addValue e v
addValue2 (I1 map i) e (IVar n)         = case e `getVarNumber` n of
                                                Nothing -> I1 (insert n i map) i
                                                Just a  -> I1 (insert n a map) i
addValue2 e1 e2   (IComp _ v1 v2)       = let e1' = addValue2 e1 e2 v1 in addValue2 e1' e2 v2
addValue2 e  _    _                     = e

addValues                               :: Extra -> [Value] -> Extra
addValues e []                          = e
addValues e (v:vs)                      = let e' = e `addValue` v in addValues e' vs

addValues2                              :: Extra -> Extra -> [Value] -> Extra
addValues2 e _  []                      = e
addValues2 e ep (v:vs)                  = let e' = addValue2 e ep v in addValues e' vs

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

mergeExtraM                             :: Extra -> Extra -> Extra
mergeExtraM Empt e                      = e
mergeExtraM e Empt                      = e
mergeExtraM (I1 m1 l) (I1 m2 r)         = I1 (unionWith f m1 m2) (if l > r then l else r)
                                                        where f i j = if i > j then i else j