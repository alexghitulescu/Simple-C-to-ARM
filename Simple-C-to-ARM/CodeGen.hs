module CodeGen (
     codeToFile,
     codeToScreen
) where

import Prelude hiding (EQ, LT, GT)
import Data.List (intercalate)
import AST
import IAST
import VMInst
import IASTCompiler
import Data.Foldable (toList)
import Data.Sequence
import qualified Data.Map as M


data Str      = T (M.Map String Int) Int

data ST a     = S { runState :: State -> (a, State) }
type State    = (Seq String, [Name], Str)

apply         :: ST a -> State -> (a,State)
apply (S f)  = f 

instance Monad ST where
      -- return :: a -> ST a
      return x   = S (\s -> (x,s))

      -- (>>=)  :: ST a -> (a -> ST b) -> ST b
      st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')

add                     :: String -> ST ()
add s                   =  S (\(seq, ns, str) -> ((), (seq |> s, ns, str)))
      
add1                    :: String -> ST ()
add1 s1                 =  S (\(seq, ns, str) -> ((), (seq |> s1 |> endl, ns, str)))

add2                    :: String -> String -> ST ()
add2 s1 s2              =  S (\(seq, ns, str) -> ((), (seq |> s1 |> s2 |> endl, ns, str)))

add3                    :: String -> String -> String -> ST ()
add3 s1 s2 s3           =  S (\(seq, ns, str) -> ((), (seq |> s1 |> s2 |> s3 |> endl, ns, str)))

add4                    :: String -> String -> String -> String -> ST ()
add4 s1 s2 s3 s4        =  S (\(seq, ns, str) -> ((), (seq |> s1 |> s2 |> s3 |> s4 |> endl, ns, str)))

add5                    :: String -> String -> String -> String -> String -> ST ()
add5 s1 s2 s3 s4 s5     =  S (\(seq, ns, str) -> ((), (seq |> s1 |> s2 |> s3 |> s4 |> s5 |> endl, ns, str)))

add6                    :: String -> String -> String -> String -> String -> String -> ST ()
add6 s1 s2 s3 s4 s5 s6  =  S (\(seq, ns, str) -> ((), (seq |> s1 |> s2 |> s3 |> s4 |> s5 |> s6 |> endl, ns, str)))
      
addVar                  :: Name -> ST ()
addVar n                =  S (\(seq, ns, str) -> ((), (seq, n:ns, str)))     
      
addEndl                 :: ST ()
addEndl                 = add " \n"

comment                 :: String -> ST ()
comment s               =  S (\(seq, ns, str) -> ((), (seq |> "\t/*" |> s |> "*/" |> endl, ns, str)))

commentB                :: String -> ST ()
commentB s              =  S (\(seq, ns, str) -> ((), (seq |> "\n\t/*" |> s |> "*/"|> endl, ns, str)))

addToFront              :: Seq String -> ST ()
addToFront s            =  S (\(seq, ns, str) -> ((), (s >< seq, ns, str)))

addToBack               :: Seq String -> ST ()
addToBack s             =  S (\(seq, ns, str) -> ((), (seq >< s, ns, str)))

addString               :: String -> ST Int
addString s             =  S (\(seq, ns, T map i) -> case M.lookup s map of
                                                        Nothing -> (i, (seq, ns, T (M.insert s i map) (i + 1)))
                                                        Just a  -> (a, (seq, ns, T map i)))

getStringList           :: ST [(String, Int)]
getStringList           =  S (\(seq, ns, T map i) -> (M.toList map, (seq, ns, T map i)))

toString                :: ST [String]
toString                = S (\(seq, ns, str) -> (toList seq, (seq , ns, str)))

endl                    :: String
endl                    = " \n"

codeToFile            :: Code -> String -> IO()
codeToFile c s        = writeFile s $ (intercalate "" (codeToARMFull c))

codeToScreen          :: Code -> IO()
codeToScreen          = toScreen . codeToARMFull

codeToARMFull         :: Code -> [String]
--codeToARMFull c       = [".data" ++ endl] ++ (addVars c (addMain (codeToARM c))) ++ [endl]
codeToARMFull c       = fst $ runState (codeToARMFull' c) (empty, [], T M.empty 0)

codeToARMFull'        :: Code -> ST [String]
codeToARMFull' c      = do mapM_ instToARM c
                           addMain
                           addToFront $ fromList [".data" ++ endl]
                           addToBack $ fromList [endl]
                           toString

codeToARM             :: Code -> [String]
codeToARM p           = fst $ runState (codeToARM' p) (empty, [], T M.empty 0)

codeToARM'            :: Code -> ST [String]
codeToARM' xs         = do mapM_ instToARM xs
                           toString

getRegVal       :: Reg -> String
getRegVal PC    = "pc"
getRegVal SB    = "r9" 
getRegVal LR    = "lr"
getRegVal SP    = "sp"
getRegVal TEMP  = "r12"
getRegVal (R n) = n

instToARM                   :: Inst -> ST ()
instToARM (ADDRESS n)       =    addVar n
instToARM (PUSHV (G n))     = do commentB ("PUSHV " ++ n)
                                 add2 "\t ldr r3, addr_" n 
                                 add1 "\t ldr r3, [r3]"
                                 add1 "\t push {r3}"
                                 comment "end"
                                 
instToARM (PUSHV r)         =    add3 "\t push {" (getRegVal r) "}"

instToARM (POP (G n))       = do commentB ("POP " ++ n)
                                 add2 "\t ldr r3, addr_" n
                                 add1 "\t pop {r4}"
                                 add1 "\t str r4, [r3]"
                                 comment "end"

instToARM (POP r)           =    add3 "\t pop {" (getRegVal r) "}"
                                 
instToARM (ADD SP r1 imd)   =    add4 "\t add sp, " (getRegVal r1) ", " (getImdVal imd (-4))
instToARM (ADD SB r1 imd)   =    add4 "\t add r12, " (getRegVal r1) ", " (getImdVal imd (-4))
instToARM (ADD rf r1 imd)   =    add6 "\t add " (getRegVal rf) ", " (getRegVal r1) ", " (getImdVal imd 1)   

instToARM (SUB SP r1 imd)   =    add4 "\t sub sp, " (getRegVal r1) ", " (getImdVal imd (-4))
instToARM (SUB SB r1 imd)   =    add4 "\t sub r12, " (getRegVal r1) ", " (getImdVal imd (-4))
instToARM (SUB rf r1 imd)   =    add6 "\t sub " (getRegVal rf) ", " (getRegVal r1) ", " (getImdVal imd 1)

instToARM (MUL rf r1 imd)   =    add6 "\t mul " (getRegVal rf) ", " (getRegVal r1) ", " (getImdVal imd 1)  

instToARM (DIV rf r1 imd)   =    add1 "\t bl __aeabi_idiv"

instToARM (MOD rf r1 imd)   =    add1 "\t bl __aeabi_idivmod"

instToARM (CMP r1 imd)      =    add4 "\t cmp " (getRegVal r1) ", " (getImdVal imd 1)

instToARM (MOV r (VAL i))   =    if(abs i <= 256) then add4 "\t mov " (getRegVal r) ", #" (show i)
                                              else add4 "\t ldr " (getRegVal r) ", =" (show i)
instToARM (MOV r (P rs 0))  =    add4 "\t mov " (getRegVal r) ", " (getRegVal rs)
instToARM (MOV r (P rs i))  =    add6 "\t add " (getRegVal r) ", " (getRegVal rs) ", #" (show (i * (-4)))

instToARM (B cond l)        =    add4 "\t b" (condToARM cond) " " (getLabel l)
instToARM (BL cond l)       =    add4 "\t bl" (condToARM cond) " " (getLabel l)
instToARM (BX cond r)       =    add4 "\t bx" (condToARM cond) " " (getRegVal r)
instToARM (LABEL l)         =    add2 (getLabel l) ":" 

instToARM (PRINT str nr)    = do commentB "PRINT"
                                 i <- addString str
                                 add2 "\t ldr r0, =s" (show i)
                                 add1 "\t bl printf"
                                 let g n | n <= 0    = return ()
                                         | otherwise = instToARM (SUB SP SP (VAL n))
                                 g (nr - 3)
                                 comment "end"

instToARM (READ rd)         = do commentB "READ"
                                 add1 "\t ldr r0, =in"
                                 add1 "\t ldr r1, =nr"
                                 add1 "\t bl scanf"
                                 add3 "\t ldr " (getRegVal rd) ", =nr"
                                 add5 "\t ldr " (getRegVal rd) ", [" (getRegVal rd) "]"
                                 comment "end"
instToARM (LDR r1 imd)      = case imd of
                                P (G n) _ -> do commentB ("LDR " ++ n ++ " in " ++ (getRegVal r1))
                                                add2 "\t ldr r3, addr_" n
                                                add3 "\t ldr " (getRegVal r1) ", [r3]"
                                                comment "end"
                                _         ->    add4 "\t ldr " (getRegVal r1) ", " (getImdVal imd (-4))
                                
instToARM (STR r1 imd)      = case imd of
                                P (G n) _ -> do commentB ("STR " ++ n ++ " from " ++ (getRegVal r1))
                                                add2 "\t ldr r3, addr_" n
                                                add3 "\t str " (getRegVal r1) ", [r3]"
                                                comment "end"
                                _         ->    add4 "\t str " (getRegVal r1) ", " (getImdVal imd (-4))
instToARM (DEBUG _)         = return ()
instToARM (BREAK _)         = return ()


getImdVal               :: Imd -> Int -> String
getImdVal (VAL i) mul   = '#':show (i * mul)
getImdVal (P r 0) mul   = getRegVal r
getImdVal (P r i) mul   = "[" ++ (getRegVal r) ++ ", #" ++ show (i * mul) ++ "]"

getLabel                :: Label -> String
getLabel (V i)          = "label" ++ show i
getLabel (N n)          = n

opToARM       :: Op -> String
opToARM Add   = "add"
opToARM Sub   = "sub"          
opToARM Mul   = "mul"
opToARM Div   = "sub"

toScreen      :: [String] -> IO()
toScreen []   = return ()
toScreen (x:xs) = do  putStr x
                      toScreen(xs)

addMain         :: ST ()
addMain         = do let f (str, i) = addToFront $ fromList [".align 2" ++ endl, ('s':(show i)) ++ ": .asciz " ++ str ++ endl]
                     list <- getStringList
                     addToFront $ fromList [".align 2" ++ endl,"in: .asciz \"%d\"" ++ endl, ".align 2" ++ endl, 
                                            "nr: .word 0" ++ endl, ".align 2" ++ endl,".text" ++ endl, ".align 2" ++ endl,
                                            ".global printf" ++ endl, ".align 2" ++ endl, ".global scanf" ++ endl, ".align 2" ++ endl, 
                                            ".global main" ++ endl, ".align 2" ++ endl, ".global __aeabi_idivmod" ++ endl, 
                                            ".global __aeabi_idiv", endl]           
                     mapM_ f list
                     addToBack  $ fromList [endl , endl]

{-addMain       :: [String] -> [String]
addMain x     = [".balign 4" ++ endl, "nr: .asciz \"%d \\n\"" ++ endl, ".balign 4" ++ endl,
                      ".text" ++ endl, ".global printf" ++ endl, 
                      ".balign 4" ++ endl, ".global main" ++ endl, ".global __aeabi_idivmod" ++ endl,
                      ".global __aeabi_idiv", endl] ++ x ++ [endl , endl, "addr_of_nr : .word nr" ++ endl, endl]-}

condToARM       :: Cond -> String
condToARM NONE  = ""
condToARM EQ    = "eq"
condToARM NE    = "ne"
condToARM GT    = "gt"
condToARM LT    = "lt"
condToARM GE    = "ge"
condToARM LE    = "le"

                      
{-addVars                       :: Code -> [String] -> [String]
addVars _                []   = []
addVars []               xs   = xs
addVars ((ADDRESS n):cs) xs   = [".balign 4" ++ endl, "var_" ++ n ++ ": .word 0" ++ endl] ++ (addVars cs xs) ++
                                      ["addr_" ++ n ++ " : .word var_" ++ n ++ endl]
addVars (_:cs)           xs   = addVars cs xs-}
