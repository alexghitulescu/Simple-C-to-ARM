{-# LANGUAGE ExistentialQuantification #-}
module VMRunner (
     execPrint
) where

import Prelude hiding (EQ, LT, GT, compare)
import Data.Array.IArray
import Data.Foldable (toList)
import Data.List
import Text.Printf
import Text.Parsec.Pos
import Control.Exception
import qualified Data.Sequence as Sq
import qualified Data.Map as M
import AST
import VMInst
import ASTCompiler
--import SampleProg

stackSize = 2000
tempReg = "scratch"
end = 10000000
debug = False

execM                   :: Code -> Bool -> IO State
execM c b               = do let index = elemIndex (LABEL (N "main")) c
                             case index of 
                                Nothing -> do putStrLn "Could not find function \"main\""
                                              return (0, 0, 0, 0, M.empty, array (0, 0) [], NONE', emptyBr b)
                                Just i  -> do result <- runState (start c) (i, 0, length c + 1, -1, M.empty, stack, NONE', emptyBr b)
                                              return $ fst result
                                                                        where stack = array (0, stackSize) [(i, 999) | i <- [0..stackSize]]

execPrint                       :: Code -> Bool -> IO ()
execPrint c b                   = do putStr "Started execution\n"
                                     (pc, sb, lr, sp, m, s, cflag, stdout) <- execM c b
                                     let stack = take (sp + 1) (assocs s)
                                     putStr ("\n(pc: " ++ show(pc) ++ ", sb: " ++ show(sb) ++ ", lr: " ++ show(lr))
                                     putStr (", sp: " ++ show(sp) ++ ", mem: " ++ show(m) ++ ", stack: " ++ show(stack))
                                     putStrLn (", flag: " ++ show(cflag))

printDebug                      :: String -> IO ()
printDebug s                    = if debug then putStrLn s
                                           else return ()

data Breaks = Br (M.Map SourcePos Bool) Bool Bool Bool Bool
emptyBr bool = Br M.empty bool False True True
                                           
-- State Monad 
-- ===========

-- Declaration for the state monad and a new type runState to save writing State -(a, State).

data ST a     = S { runState :: State -> IO (a, State) }
type State    = (Int, Int, Int, Int, Mem, Stack Int, CFlag, Breaks)

instance Monad ST where
      -- return :: a -> ST a
      return a    = S $ \st -> return (a, st)
      
      -- (>>=)  :: ST a -> (a -> ST b) -> ST b
      (>>=) stf f = S $ \st -> do (a1, st1) <- runState stf st
                                  (a2, st2) <- runState (f a1) st1
                                  return (a2, st2)

liftIO :: IO a -> ST a
liftIO io = S $ \st -> do x <- io
                          return (x, st)                                  

next          :: ST ()
next          =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return ((), (pc + 1, sb, lr, sp, m, s, cflag, stdout)))

{-prev          :: ST ()
prev          =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return ((), (pc - 1, sb, lr, sp, m, s, cflag, stdout)))

nothing       :: ST ()
nothing       =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return ((), (pc, sb, lr, sp, m, s, cflag, stdout)))-}

pop           :: ST Int
pop           =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> if sp < 0 then do putStrLn ("pop; stack underflow: " ++ show sp)
                                                                                return (s ! 0, (end, sb, lr, 0, m, s, cflag, stdout))
                                                                        else return (s ! sp, (pc, sb, lr, sp - 1, m, s, cflag, stdout)))

push          :: Int -> ST ()
push i        =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> if sp< -1 then do putStrLn ("push; stack underflow: " ++ show sp)
                                                                                return ((), (end, sb, lr, sp + 1, m, s, cflag, stdout))
                                                                        else return ((), (pc, sb, lr, sp + 1, m, s // [(sp + 1, i)], cflag, stdout)))

put           :: (Name, Int) -> ST ()
put (n, i)    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return ((), (pc, sb, lr, sp, M.insert n i m, s, cflag, stdout)))

retrieve      :: Code -> ST Inst
retrieve c    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> if pc < 0 then do putStrLn ("negative program counter: " ++ show pc)
                                                                                return (HALT, (pc, sb, lr, sp, m, s, cflag, stdout))
                                                                        else return (if pc < length c then c !! pc
                                                                                                           else HALT, (pc, sb, lr, sp, m, s, cflag, stdout)))

jump          :: Code -> Label -> ST ()
jump c l      =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> do let index = elemIndex (LABEL l) c
                                                                 case index of 
                                                                        Nothing -> do putStrLn ("could not find label: " ++ show l)
                                                                                      return ((), (end, sb, lr, sp, m, s, cflag, stdout))
                                                                        Just i  -> return ((), (i, sb, lr, sp, m, s, cflag, stdout)))

jumpI          :: Code -> Int -> ST ()
jumpI c i      =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return ((), (i, sb, lr, sp, m, s, cflag, stdout)))

mem           :: ST Mem
mem           =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return (m, (pc, sb, lr, sp, m, s, cflag, stdout)))

state         :: ST State
state         =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return ((pc, sb, lr, sp, m, s, cflag, stdout), (pc, sb, lr, sp, m, s, cflag, stdout)))

savePCToLR    :: ST ()
savePCToLR    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return ((), (pc, sb, pc, sp, m, s, cflag, stdout)))

setCflag      :: CFlag -> ST ()
setCflag flag =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return ((), (pc, sb, lr, sp, m, s, flag, stdout)))

validCflag    :: Cond -> ST Bool
validCflag cf =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return (compareCF cflag cf, (pc, sb, lr, sp, m, s, cflag, stdout)))

getRegVal       :: Reg -> ST Int
getRegVal PC    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return (pc, (pc, sb, lr, sp, m, s, cflag, stdout)))
getRegVal SB    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return (sb, (pc, sb, lr, sp, m, s, cflag, stdout)))
getRegVal LR    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return (lr, (pc, sb, lr, sp, m, s, cflag, stdout)))
getRegVal SP    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return (sp, (pc, sb, lr, sp, m, s, cflag, stdout)))
getRegVal TEMP  =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> case M.lookup tempReg m of 
                                                                        Nothing -> do putStr $ "could not find temp register\n"
                                                                                      return (99999, (pc, sb, lr, sp, m, s, cflag, stdout))
                                                                        Just a  -> return (a, (pc, sb, lr, sp, m, s, cflag, stdout)))
getRegVal (R n) =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> case M.lookup n m of 
                                                                        Nothing -> do putStr $ "could not find register: " ++ n ++ "\n"
                                                                                      return (99999, (pc, sb, lr, sp, m, s, cflag, stdout))
                                                                        Just a  -> return (a, (pc, sb, lr, sp, m, s, cflag, stdout)))
getRegVal (G n) =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> case M.lookup n m of 
                                                                        Nothing -> do putStr $ "could not find global: " ++ n ++ "\n"
                                                                                      return (99999, (pc, sb, lr, sp, m, s, cflag, stdout))
                                                                        Just a  -> return (a, (pc, sb, lr, sp, m, s, cflag, stdout)))

setRegVal         :: Reg -> Int -> ST ()
setRegVal PC i    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return ((), (i, sb, lr, sp, m, s, cflag, stdout)))
setRegVal SB i    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return ((), (pc, i, lr, sp, m, s, cflag, stdout)))
setRegVal LR i    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return ((), (pc, sb, i, sp, m, s, cflag, stdout)))
setRegVal SP i    =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return ((), (pc, sb, lr, i, m, s, cflag, stdout)))
setRegVal TEMP i  =  put (tempReg, i)
setRegVal (R n) i =  put (n, i)
setRegVal (G n) i =  put (n, i)

load            :: Int -> ST Int
load pos        =  if pos < 0 then S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> do putStr ("loading off stack: " ++ show pos ++ "\n")
                                                                                   return (0, (end, sb, lr, sp, m, s, cflag, stdout)))
                              else S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return (s ! pos, (pc, sb, lr, sp, m, s, cflag, stdout)))
--load i          =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return (s ! i, (pc, sb, lr, sp, m, s, cflag, stdout)))

store           :: Int -> Int -> ST ()
store pos i     =  if pos < 0 then S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> do putStr ("storing off stack: " ++ show pos ++ "\n")
                                                                                   return ((), (end, sb, lr, sp, m, s, cflag, stdout)))
                              else S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return ((), (pc, sb, lr, sp, m, s // [(pos, i)], cflag, stdout)))
--store pos i     =  S (\(pc, sb, lr, sp, m, s, cflag, stdout) -> return ((), (pc, sb, lr, sp, m, s // [(pos, i)], cflag, stdout)))

start           :: Code -> ST State
start c         = do addAddress (filter (isAddress) c)
                     let f (n,i) = put (n,i)
                     mapM_ put [("r4",-991),("r5",-991),("r6",-991),("r7",-991),("r8",-991),("r10",-991),("r11",-991)]
                     execCode c
                     
isDebug         :: ST Bool 
isDebug         = S (\(pc, sb, lr, sp, m, s, cflag, Br map debug step print run) -> return (debug, (pc, sb, lr, sp, m, s, cflag, Br map debug step print run)))

stopDebug       :: ST ()
stopDebug       = S (\(pc, sb, lr, sp, m, s, cflag, Br map _ _ _ _) -> return ((), (pc, sb, lr, sp, m, s, cflag, Br map False False False True))) 

startStep       :: ST ()
startStep       = S (\(pc, sb, lr, sp, m, s, cflag, Br map debug _ print run) -> return ((), (pc, sb, lr, sp, m, s, cflag, Br map debug True print run))) 

isStep          :: ST Bool
isStep          = S (\(pc, sb, lr, sp, m, s, cflag, Br map debug step print run) -> return (step, (pc, sb, lr, sp, m, s, cflag, Br map debug step print run)))  

stopStep        :: ST ()
stopStep        = S (\(pc, sb, lr, sp, m, s, cflag, Br map debug _ print run) -> return ((), (pc, sb, lr, sp, m, s, cflag, Br map debug False print run))) 

insertP         :: SourcePos -> ST Bool
insertP pos     = S (\(pc, sb, lr, sp, m, s, cflag, Br map debug step pr run) -> case M.lookup pos map of
                                                                                   Nothing -> return (True, (pc, sb, lr, sp, m, s, cflag, 
                                                                                                                 Br (M.insert pos True map) debug step pr run))
                                                                                   Just a  -> return (a, (pc, sb, lr, sp, m, s, cflag, 
                                                                                                                                    Br map debug step pr run)))  

insertR         :: SourcePos -> ST ()
insertR pos     = S (\(pc, sb, lr, sp, m, s, cflag, Br map debug step pr run) -> return ((), (pc, sb, lr, sp, m, s, cflag, 
                                                                                                               Br (M.insert pos False map) debug step pr run)))

isPrint         :: ST Bool
isPrint         = S (\(pc, sb, lr, sp, m, s, cflag, Br map debug step print run) -> if print
                                                                                        then return (print, (pc, sb, lr, sp, m, s, cflag, 
                                                                                                                                  Br map debug step print run))
                                                                                        else return (False, (pc, sb, lr, sp, m, s, cflag, 
                                                                                                                                  Br map debug step True run)))

noPrint         :: ST ()
noPrint         = S (\(pc, sb, lr, sp, m, s, cflag, Br map debug step _ run) -> return ((), (pc, sb, lr, sp, m, s, cflag, Br map debug step False run)))

isRun         :: ST Bool
isRun         = S (\(pc, sb, lr, sp, m, s, cflag, Br map debug step print run) -> if run
                                                                                        then return (run, (pc, sb, lr, sp, m, s, cflag, 
                                                                                                                                  Br map debug step print run))
                                                                                        else return (False, (pc, sb, lr, sp, m, s, cflag, 
                                                                                                                                Br map debug step print True)))

noRun         :: ST ()
noRun         = S (\(pc, sb, lr, sp, m, s, cflag, Br map debug step print _) -> return ((), (pc - 1, sb, lr, sp, m, s, cflag, Br map debug step print False)))
                                                                                 
processDebug            :: String -> ST ()
processDebug "mem"      = do m <- mem
                             liftIO $ putStrLn (show m)
                             noRun
                             noPrint
processDebug "stack"    = do (_, _, _, sp, _, s, _, _) <- state
                             let stack = take (sp + 1) (assocs s)
                             liftIO $ putStrLn (show stack)
                             noRun
                             noPrint
processDebug "run"      = stopDebug
processDebug "con"      = stopStep
processDebug "step"     = return ()
processDebug "help"     = do liftIO $ do putStrLn "mem: prints the contents of the memory"
                                         putStrLn "stack: prints the contents of the stack"
                                         putStrLn "run: resumes the program, ignoring any other break points"
                                         putStrLn "con: resumes the program"
                                         putStrLn "step: step through the program"
                                         putStrLn "rem: when on a BREAK instructions, the BREAK instruction gets deactivated"
                             noRun
                             noPrint
processDebug x          = do liftIO $ putStrLn "unknown command. type \"help\" for instructions"
                             noRun
                             noPrint

processDebug2           :: String -> SourcePos -> ST ()
processDebug2 "rem" p   = do liftIO $ putStrLn ("BREAK " ++ show p ++ " deactivated")
                             insertR p
                             noRun
                             noPrint
processDebug2 l     _   = processDebug l

execCode    :: Code -> ST State
execCode c  = do inst <- retrieve c
                 next
                 m <- mem
                 (_, _, _, sp, _, s, _, _) <- state
                 let stack = take (sp + 1) (assocs s)
                 liftIO $ printDebug ("\t\t\t | " {-++ show m ++ "|"-} ++ show stack ++ "\n>" ++ show inst)
                 step <- isStep
                 if step 
                    then do print <- isPrint
                            if print then liftIO $ putStrLn ("next: " ++ show inst)
                                     else return ()
                            line <- liftIO $ do putStr "> "
                                                getLine
                            case inst of 
                                   (BREAK pos) -> processDebug2 line pos
                                   _           -> processDebug line
                    else 
                         case inst of      
                                  (BREAK pos) -> do b1 <- insertP pos
                                                    b2 <- isDebug
                                                    if b1 && b2 then do liftIO $ putStrLn (show $ BREAK pos)
                                                                        startStep
                                                                        line <- liftIO $ do putStr "> "
                                                                                            getLine
                                                                        processDebug2 line pos
                                                                else return ()
                                  _           -> return ()
                 run <- isRun
                 if not run
                        then execCode c
                        else case inst of 
                                  (BREAK pos)             -> execCode c
                                  (PUSHV r)               -> do val <- getRegVal r
                                                                push val
                                                                execCode c
                                                                
                                  (POP r)                 -> do val <- pop
                                                                setRegVal r val
                                                                execCode c
                                                                
                                  (ADD rf r1 imd)         -> do val1 <- getRegVal r1
                                                                val2 <- getImdVal imd
                                                                setRegVal rf (val1 + val2)
                                                                execCode c
                                   
                                  (SUB rf r1 imd)         -> do val1 <- getRegVal r1
                                                                val2 <- getImdVal imd
                                                                setRegVal rf (val1 - val2)
                                                                execCode c
                                                                
                                  (MUL rf r1 imd)         -> do val1 <- getRegVal r1
                                                                val2 <- getImdVal imd
                                                                setRegVal rf (val1 * val2)
                                                                execCode c
                                                                
                                  (DIV rf r1 r2)          -> do val1 <- getRegVal r1
                                                                val2 <- getRegVal r2
                                                                setRegVal rf (val1 `div` val2)
                                                                execCode c
                                   
                                  (MOD rf r1 r2)          -> do val1 <- getRegVal r1
                                                                val2 <- getRegVal r2
                                                                setRegVal rf (val1 `mod` val2)
                                                                execCode c 
                                   
                                  (MOV r imd)             -> do val <- getImdVal imd
                                                                setRegVal r val
                                                                execCode c
                                   
                                  (BX cond reg)           -> execInstBX c cond reg
                                                                
                                  (B cond l)              -> execInstB c cond l
                                  (BL cond l)             -> do savePCToLR
                                                                execInstB c cond l
                                                                
                                  (LABEL l)               -> execCode c
                                                                
                                  (PRINT str i)           -> do vs <- getValues i
                                                                let vs' = map Pt $ reverse vs
                                                                let str' = read str
                                                                liftIO $ do result <- try  $ putStr . printfa str' $ vs' :: IO (Either SomeException ())
                                                                            case result of
                                                                              Left e  -> putStrLn $ ' ':(show e)
                                                                              Right _ -> return ()
                                                                execCode c

                                  (READ reg)              -> do nr <- liftIO $ do number <- getLine
                                                                                  result <- try  $ evaluate (read number) :: IO (Either SomeException Int)
                                                                                  case result of
                                                                                     Left _  -> return 0
                                                                                     Right n -> return n
                                                                setRegVal reg nr
                                                                execCode c
                                                                
                                  (HALT)                  -> do s <- state
                                                                return s
                                                                
                                  (LDR r1 (P r2 d))       -> do r2val <- getRegVal r2
                                                                case r2 of
                                                                        G n -> do setRegVal r1 r2val
                                                                                  execCode c
                                                                        _   -> do val <- load (r2val + d)
                                                                                  setRegVal r1 val
                                                                                  execCode c
                                                                
                                  (LDRV r i)              -> do setRegVal r i
                                                                execCode c
                                                                
                                  (STR r1 (P r2 d))       -> do r1val <- getRegVal r1
                                                                case r2 of
                                                                        G n -> do setRegVal r2 r1val 
                                                                                  execCode c
                                                                        _   -> do r2val <- getRegVal r2
                                                                                  store (r2val + d) r1val
                                                                                  execCode c
                                                                
                                  (CMP r1 imd)            -> do r1val <- getRegVal r1
                                                                imdval <- getImdVal imd
                                                                --liftIO $ printDebug (":::::" ++ show r1val ++ " vs " ++ show imdval ++ ":::::")
                                                                setCflag (compare r1val imdval)
                                                                execCode c
                                  (DEBUG s)               -> execCode c
                                  (other)                 -> do liftIO $ putStrLn ("other: " ++ show other)
                                                                setRegVal PC end
                                                                execCode c


addAddress                      :: Code -> ST ()
addAddress []                   = return ()
addAddress (ADDRESS n:xs)       = do put (n, 0)
                                     addAddress xs

getImdVal               :: Imd -> ST Int
getImdVal (VAL i)       = return i
getImdVal (P r i)       = do val <- getRegVal r
                             return (val + i)
         
getValues               :: Int -> ST [Int]
getValues i | i == 0    = return []         
            | i <= 3    = do v <- getRegVal (R $ "r" ++ show i)
                             vs <- getValues (i - 1)
                             return $ v:vs
            | otherwise = do vs <- getValues (i - 1)
                             v <- pop
                             return $ v:vs
         
execInstBX              :: Code -> Cond -> Reg -> ST State
execInstBX c cond reg   = do b <- validCflag cond
                             if b then
                                do pos <- getRegVal reg
                                   --liftIO $ putStr ("jumping to position: " ++ show pos ++ "\n")
                                   jumpI c pos
                             else 
                                return ()
                             execCode c
                      
execInstB               :: Code -> Cond -> Label -> ST State
execInstB c cond l      = do b <- validCflag cond
                             if b then
                                do --liftIO $ putStr ("jumping to label: " ++ show l ++ "\n")
                                   jump c l
                             else 
                                return ()
                             execCode c

compare                         :: Int -> Int -> CFlag
compare i1 i2   | i1 == i2      = EQ'
                | i1 <  i2      = LT'
                | i1 >  i2      = GT'

compareCF                :: CFlag -> Cond -> Bool
compareCF _ NONE         = True
compareCF NONE' _        = False
compareCF EQ' EQ         = True
compareCF EQ' GE         = True
compareCF EQ' LE         = True
compareCF EQ' _          = False
compareCF GT' GT         = True
compareCF GT' GE         = True
compareCF GT' NE         = True
compareCF GT' _          = False
compareCF LT' LT         = True
compareCF LT' LE         = True
compareCF LT' NE         = True
compareCF LT' _          = False

remP           :: Eq a => a -> [(a,b)] -> [(a,b)]
remP _ [    ]  = []
remP n (x:xs)  = if (fst x) == n then xs else x : (remP n xs)

compNr          :: Op -> Int -> Int -> Int
compNr Add m n  = m + n
compNr Sub m n  = n - m
compNr Mul m n  = m * n
compNr Div m n  = m `quot` n

isAddress               :: Inst -> Bool
isAddress (ADDRESS _)   = True
isAddress _             = False

data PrintfArgT = forall a. PrintfArg a => Pt a

printfa :: PrintfType t => String -> [ PrintfArgT ] -> t
printfa format = printfa' format . reverse
  where printfa' :: PrintfType t => String -> [ PrintfArgT ] -> t
        printfa' format [] = printf format
        printfa' format (Pt a:as) = printfa' format as a