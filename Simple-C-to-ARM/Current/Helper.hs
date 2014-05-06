
module Helper (
    isVal,
    isVal2,
    ppIProg,
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
                                      in case nr of 0         -> E_STMT
                                                    1         -> index elem 0
                                                    otherwise -> ISeqn (toList $ cleanIStmt x)
cleanSeqn (ISeqnE x e)          = let elem = cleanIStmt x
                                      nr   = Seq.length elem
                                      in case nr of 0         -> E_STMT
                                                    otherwise -> ISeqnE (toList $ cleanIStmt x) e
cleanSeqn x                     = x

cleanIStmt                           :: [IStmt] -> Seq IStmt
cleanIStmt []                        = empty
cleanIStmt ((IIf v x y e):xs)        = (IIf v (cleanSeqn x) (cleanSeqn y) e) <| cleanIStmt xs
cleanIStmt ((IWhile ls v x e e2):xs) = (IWhile (toList $ cleanIStmt ls) v (cleanSeqn x) e e2) <| cleanIStmt xs
cleanIStmt ((ISeqn x):xs)            = cleanIStmt x >< cleanIStmt xs
cleanIStmt ((ISeqnE x e):xs)         = (ISeqnE (toList $ cleanIStmt x) e) <| cleanIStmt xs
cleanIStmt (x:xs)                    = x <| cleanIStmt xs

cleanIProg                      :: IProg -> IProg
cleanIProg (IFun n ns st e)     = IFun n ns (cleanSeqn st) e
cleanIProg (IPSeq xs)           = IPSeq (map cleanIProg xs)
cleanIProg x                    = x



ppIProg                         :: IProg -> IO()
ppIProg (IFun n ns st e)        = do putStrLn $ "IFun " ++ show n ++ show ns ++ "(Extra: " ++ show e ++ ") {"
                                     ppIStmt st 1
                                     putStrLn "}"
ppIProg (IPSeq xs)              = do putStrLn "IPSeq ["
                                     ppList1 xs
                                     putStrLn "]"
ppIProg x                       = putStrLn $ show x

ppIStmt                         :: IStmt -> Int -> IO ()
ppIStmt (ISeqn  xs) i           = do putStrLn $ tabs i ++ "ISeqn ["
                                     ppList2 xs (i + 1)
                                     putStrLn $ tabs i ++ "]"
ppIStmt (ISeqnE xs e) i         = do putStrLn $ tabs i ++ "ISeqnE (Extra1: " ++ show e ++ ") ["
                                     ppList2 xs (i + 1)
                                     putStrLn $ tabs i ++ "]"
ppIStmt (IWhile es v p e1 e2) i = do putStrLn $ tabs i ++ "IWhile (Extra1: " ++ show e1 ++ ")"
                                     putStrLn $ tabs i ++ "       (Extra2: " ++ show e2 ++ ") ["
                                     ppList2 es (i + 1)
                                     putStrLn $ tabs i ++ "]"
                                     putStrLn $ tabs i ++ show v ++ " {"
                                     ppIStmt p (i + 1)
                                     putStrLn $ tabs i ++ "}"
ppIStmt (IIf v p1 p2 e) i       = do putStrLn $ tabs i ++ "IIf (" ++ show v ++ ") (Extra: " ++ show e ++ ") {"
                                     ppIStmt p1 (i + 1)
                                     putStrLn $ tabs i ++ "} else {"
                                     ppIStmt p2 (i + 1)
                                     putStrLn $ tabs i ++"}"
ppIStmt x               i       = putStrLn $ tabs i ++ show x

ppList1                         :: [IProg] -> IO()
ppList1 []                      = return ()
ppList1 (x:xs)                  = do ppIProg x
                                     ppList1 xs

ppList2                         :: [IStmt] -> Int -> IO()
ppList2 [] _                    = return ()
ppList2 (x:xs) i                = do ppIStmt x i
                                     ppList2 xs i
                                     
tabs i = Prelude.replicate (i * 3) ' '