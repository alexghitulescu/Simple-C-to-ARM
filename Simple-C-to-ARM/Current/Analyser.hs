module Analyser (
   analyse
) where

import Prelude hiding (EQ)
import qualified Data.Map as M
import IAST
import Environment
import Extra

analyse                         :: IProg -> IProg 
analyse p                       = infoProg p


infoProg                        :: IProg -> IProg
infoProg (IGlobalVar n)         = IGlobalVar n
infoProg (IFun n ns st e)       = let (ex, st') = infoStmt st e in IFun n ns st' ex
infoProg (IPSeq xs)             = let xs' = map infoProg xs in IPSeq xs'

analyseSeqn                     :: [IStmt] -> Extra -> (Extra, [IStmt])
analyseSeqn [] _                = (Empt, [])
analyseSeqn (x:xs) e            = let (ex1, sts) = analyseSeqn xs e in
                                  let (ex2, st') = infoStmt x ex1 in (ex2, st':sts)

infoStmt                        :: IStmt -> Extra -> (Extra, IStmt)
infoStmt (ILocalVar n e) en     = case en `getVarNumber` n of
                                                Nothing -> (en, E_STMT)
                                                Just i  -> let en' = en `incNumber` 1 in
                                                           (en', ILocalVar n (let e' = e `copyNumber` en' in (e' `addName` n) i))
infoStmt (IAssign n val e) en   = case en `getVarNumber` n of
                                                Nothing -> (en, E_STMT)
                                                Just i  -> let en' = en `incNumber` 1 in
                                                           let e' = e `copyNumber` en' in
                                                           let e'' = e' `addValue` val in
                                                           (en' `addValue` val, IAssign n val ((e'' `addName` n) i))
infoStmt (IPrint val e) en      = let en' = en `incNumber` 1 in
                                  let e' = e `copyNumber` en' in 
                                  (en' `addValue` val, IPrint val (e' `addValue` val))
infoStmt (ISeqn  []) en         = (en, E_STMT)
infoStmt (ISeqnE []) en         = (en, E_STMT)
infoStmt  E_STMT en             = (en, E_STMT)
infoStmt (ISeqn  xs) en         = let (e1, sts) = analyseSeqn xs en in (e1, ISeqn sts)
infoStmt (ISeqnE xs) en         = let (e1, sts) = analyseSeqn xs en in (e1, ISeqnE sts)
infoStmt (IWhile ps v p e) en   = let (e1, p') = infoStmt p en in
                                  let e1' = (e1 `incNumber` 1) `addValue` v in
                                  let e' = e `copyNumber` e1' in
                                  let (e2, ps') = analyseSeqn ps e1' in (e2, IWhile ps v p e')
infoStmt (IIf v p1 E_STMT e) en = let (e1, p') = infoStmt p1 en in
                                  let e1' = e1 `incNumber` 1 in
                                  let e' = e `copyNumber` e1' in
                                  (e1' `addValue` v, IIf v p1 E_STMT (e' `addValue` v))
infoStmt (IIf v p1 p2 e) en     = let (e1, p1') = infoStmt p1 en in
                                  let (e2, p2') = infoStmt p2 en in
                                  let es = e1 `mergeExtraL` e2 in
                                  let es' = es `incNumber` 1 in
                                  let e' = e `copyNumber` es' in
                                  (es' `addValue` v, IIf v p1' p2' (e' `addValue` v))
infoStmt (IReturn v e) en       = (en `addValue` v, IReturn v (e `addValue` v))
infoStmt (IApply n vs r e) en   = let en' = en `incNumber` 1 in
                                  let e' = e `copyNumber` en' in
                                  let e'' = e' `addValues` vs in
                                  (e'' `mergeExtraR` en', IApply n vs r e'')
infoStmt (IApp n op v1 v2 e) en = case en `getVarNumber` n of
                                        Nothing -> (en, E_STMT)
                                        Just i  -> let en' = en `incNumber` 1 in
                                                   let e' = e `copyNumber` en' in
                                                   let e'' = e' `addValue` v1 in
                                                   let e''' = e'' `addValue` v2 in
                                                   (e''' `mergeExtraR` en', IApp n op v1 v2 (addName e''' n i))