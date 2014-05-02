
module Helper (
    isVal,
    isVal2,
) where

import IAST

isVal                           :: Value -> Bool
isVal (IVal _)                  = True
isVal _                         = False
                                             
isVal2                          :: Value -> Value -> Bool
isVal2 v1 v2                    = isVal v1 && isVal v2
