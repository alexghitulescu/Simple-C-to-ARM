
module Helper (
    isVal,
    isVal2,
    cleanIProg
) where

import Data.Foldable (toList)
import Data.Sequence as Seq
import IAST

isVal                           :: Value -> Bool
isVal (IVal _)                  = True
isVal _                         = False
                                             
isVal2                          :: Value -> Value -> Bool
isVal2 v1 v2                    = isVal v1 && isVal v2


cleanSeqn                       :: IStmt -> IStmt
cleanSeqn (ISeqn x e)           = let elem = cleanIStmt x
                                      nr   = Seq.length elem
                                      in case nr of 0         -> E_STMT
                                                    1         -> index elem 0
                                                    otherwise -> ISeqn (toList $ cleanIStmt x) e
cleanSeqn (ISeqnE x e)          = let elem = cleanIStmt x
                                      nr   = Seq.length elem
                                      in case nr of 0         -> E_STMT
                                                    otherwise -> ISeqnE (toList $ cleanIStmt x) e
cleanSeqn x                     = x

cleanIStmt                           :: [IStmt] -> Seq IStmt
cleanIStmt []                        = empty
cleanIStmt ((IIf v x y e):xs)        = (IIf v (cleanSeqn x) (cleanSeqn y) e) <| cleanIStmt xs
cleanIStmt ((IWhile ls v x e):xs)    = (IWhile (toList $ cleanIStmt ls) v (cleanSeqn x) e) <| cleanIStmt xs
cleanIStmt ((ISeqn x _):xs)          = cleanIStmt x >< cleanIStmt xs
cleanIStmt ((ISeqnE x e):xs)         = (ISeqnE (toList $ cleanIStmt x) e) <| cleanIStmt xs
cleanIStmt (x:xs)                    = x <| cleanIStmt xs

cleanIProg                      :: IProg -> IProg
cleanIProg (IFun n ns st e)     = IFun n ns (cleanSeqn st) e
cleanIProg (IPSeq xs e)         = IPSeq (map cleanIProg xs) e
cleanIProg x                    = x