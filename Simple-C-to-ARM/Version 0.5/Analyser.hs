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
analyseSeqn [] e                = (e, [])
analyseSeqn (x:xs) e            = let (ex1, sts) = analyseSeqn xs e in
                                  let (ex2, st') = infoStmt x ex1 in (ex2, st':sts)

infoStmt                        :: IStmt -> Extra -> (Extra, IStmt)
infoStmt (ILocalVar n e) en     = let en' = en `incNumber` 1 in
                                  case en' `getVarNumber` n of
                                                Nothing -> (en', E_STMT)
                                                Just i  -> (en', ILocalVar n (let e' = e `copyNumber` en' in (e' `addName` n) i))
infoStmt (IAssign n val e) en   = let en' = en `incNumber` 1 in
                                  case en' `getVarNumber` n of
                                                Nothing -> (en', E_STMT)
                                                Just i  -> let e' = e `copyNumber` en' in
                                                           let e'' = addValue2 e' en' val in
                                                           (en' `addValue` val, IAssign n val ((e'' `addName` n) i))
infoStmt (IPrint val e) en      = let en' = en `incNumber` 1 in
                                  let e' = e `copyNumber` en' in 
                                  (en' `addValue` val, IPrint val (addValue2 e' en' val))
infoStmt (ISeqn  []) en         = (en, E_STMT)
infoStmt (ISeqnE [] _) en       = (en, E_STMT)
infoStmt  E_STMT en             = (en, E_STMT)
infoStmt (ISeqn  xs) en         = let en' = en `incNumber` 1 in let (e1, sts) = analyseSeqn xs en' in (e1, ISeqn sts)
infoStmt (ISeqnE xs e) en       = let en' = en `incNumber` 1 in let (e1, sts) = analyseSeqn xs en' in (e1, ISeqnE sts en')
infoStmt (IWhile ps v p e e2) en= let en' = (en `incNumber` 1) `addValue` v in
                                  let (e1, p') = infoStmt p en' in
                                  let e1' = (e1 `addValue` v) `incNumber` 1 in
                                  let (e1'', ps') = analyseSeqn ps e1' in (e1'', IWhile ps' v p' e1'' e1)
infoStmt (IIf v p1 E_STMT e) en = let en' = en `incNumber` 1 in
                                  let (e1, p1') = infoStmt p1 en' in
                                  let e1' = e1 `incNumber` 1 in
                                  let e' = e `copyNumber` e1' in
                                  (e1' `addValue` v, IIf v p1' E_STMT (addValue2 e' e1' v))
infoStmt (IIf v p1 p2 e) en     = let en' = en `incNumber` 1 in
                                  let (e1, p1') = infoStmt p1 en' in
                                  let (e2, p2') = infoStmt p2 en' in
                                  let es = e1 `mergeExtraM` e2 in
                                  let es' = es `incNumber` 1 in
                                  let e' = e `copyNumber` es' in
                                  (es' `addValue` v, IIf v p1' p2' (addValue2 e' es' v))
infoStmt (IReturn v e) en       = let en' = en `incNumber` 1 in
                                  let e' = e `copyNumber` en' in
                                  (en' `addValue` v, IReturn v (addValue2 e' en' v))
infoStmt (IApply n vs r e) en   = let en' = en `incNumber` 1 in
                                  let e' = e `copyNumber` en' in
                                  let e1'' = e' `addValues` vs in
                                  let e2'' = addValues2 e' en' vs in
                                  case en' `getVarNumber` r of
                                        Nothing -> (e1'' `mergeExtraR` en', IApply n vs r e2'')
                                        Just i  -> (e1'' `mergeExtraR` en', IApply n vs r (addName e2'' r i))
infoStmt (IApp n op v1 v2 e) en = let en' = en `incNumber` 1 in
                                  case en' `getVarNumber` n of
                                        Nothing -> (en', E_STMT)
                                        Just i  -> let e' = e `copyNumber` en' in
                                                   let e1'' = (e' `addValue` v1) `addValue` v2 in
                                                   let e2'' = addValue2 (addValue2 e' en' v1) en' v2 in
                                                   --let I1 map i' = en' in 
                                                   (e1'' `mergeExtraR` en', IApp n op v1 v2 (addName e2'' n i))