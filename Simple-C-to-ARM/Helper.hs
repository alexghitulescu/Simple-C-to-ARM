
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
                                     ppIStmt 1 st
                                     putStrLn "}"
ppIProg (IPSeq xs)              = do putStrLn "IPSeq ["
                                     mapM_ ppIProg xs
                                     putStrLn "]"
ppIProg x                       = putStrLn $ show x

ppIStmt                         :: Int -> IStmt -> IO ()
ppIStmt i (ISeqn  xs)           = do putStrLn $ tabs i ++ "ISeqn ["
                                     mapM_ (ppIStmt (i + 1)) xs
                                     putStrLn $ tabs i ++ "]"
ppIStmt i (ISeqnE xs e)         = do putStrLn $ tabs i ++ "ISeqnE (Extra1: " ++ show e ++ ") ["
                                     mapM_ (ppIStmt (i + 1)) xs
                                     putStrLn $ tabs i ++ "]"
ppIStmt i (IWhile es v p e1 e2) = do putStrLn $ tabs i ++ "IWhile (Extra1: " ++ show e1 ++ ")"
                                     putStrLn $ tabs i ++ "       (Extra2: " ++ show e2 ++ ") ["
                                     mapM_ (ppIStmt (i + 1)) es
                                     putStrLn $ tabs i ++ "]"
                                     putStrLn $ tabs i ++ show v ++ " {"
                                     ppIStmt (i + 1) p
                                     putStrLn $ tabs i ++ "}"
ppIStmt i (IIf v p1 p2 e)       = do putStrLn $ tabs i ++ "IIf (" ++ show v ++ ") (Extra: " ++ show e ++ ") {"
                                     ppIStmt (i + 1) p1
                                     putStrLn $ tabs i ++ "} else {"
                                     ppIStmt (i + 1) p2
                                     putStrLn $ tabs i ++"}"
ppIStmt i  x                    = putStrLn $ tabs i ++ show x
                                     
tabs i = Prelude.replicate (i * 3) ' '