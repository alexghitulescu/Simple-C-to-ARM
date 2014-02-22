 import Control.Monad.ST
 import Data.Array.ST
 
 buildPair = do arr <- newArray (1,10) 37 :: ST s (STArray s Int Int)
                a <- readArray arr 1
                writeArray arr 1 64
                b <- readArray arr 1
                return (a,b)
 
 main = print $ runST buildPair