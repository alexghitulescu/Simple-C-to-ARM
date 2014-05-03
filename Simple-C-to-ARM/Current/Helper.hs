
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
cleanSeqn (ISeqn x)             = let elem = cleanIStmt x
                                      nr   = Seq.length elem
                                      in case nr of 0         -> EMPTY_STMT
                                                    1         -> index elem 0
                                                    otherwise -> ISeqn (toList $ cleanIStmt x)
cleanSeqn (ISeqnE x)            = let elem = cleanIStmt x
                                      nr   = Seq.length elem
                                      in case nr of 0         -> EMPTY_STMT
                                                    otherwise -> ISeqnE (toList $ cleanIStmt x)
cleanSeqn x                     = x

cleanIStmt                           :: [IStmt] -> Seq IStmt
cleanIStmt []                        = empty
cleanIStmt ((IIf v x y):xs)          = (IIf v (cleanSeqn x) (cleanSeqn y)) <| cleanIStmt xs
cleanIStmt ((IWhile ls v x):xs)      = (IWhile (toList $ cleanIStmt ls) v (cleanSeqn x)) <| cleanIStmt xs
cleanIStmt ((ISeqn x):xs)            = cleanIStmt x >< cleanIStmt xs
cleanIStmt ((ISeqnE x):xs)           = (ISeqnE $ toList(cleanIStmt x)) <| cleanIStmt xs
cleanIStmt (x:xs)                    = x <| cleanIStmt xs

cleanIProg                      :: IProg -> IProg
cleanIProg (IFun n ns st)       = IFun n ns (cleanSeqn st)
cleanIProg (IPSeq xs)           = IPSeq (map cleanIProg xs)
cleanIProg x                    = x