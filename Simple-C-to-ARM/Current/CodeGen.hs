
module CodeGen (
     progToFile,
     progToScreen
) where

import Prelude hiding (EQ, LT, GT, empty)
import Data.List (intercalate)
import AST
import VMInst
import ASTCompiler
import Data.Foldable (toList)
import Data.Sequence




data ST a     = S { runState :: State -> (a, State) }
type State    = (Seq String, [Name])

apply         :: ST a -> State -> (a,State)
apply (S f)  = f 

instance Monad ST where
      -- return :: a -> ST a
      return x   = S (\s -> (x,s))

      -- (>>=)  :: ST a -> (a -> ST b) -> ST b
      st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')

add                     :: String -> ST ()
add s                   =  S (\(seq, ns) -> ((), (seq |> s, ns)))
      
add1                    :: String -> ST ()
add1 s1                 =  S (\(seq, ns) -> ((), (seq |> s1 |> endl, ns)))

add2                    :: String -> String -> ST ()
add2 s1 s2              =  S (\(seq, ns) -> ((), (seq |> s1 |> s2 |> endl, ns)))

add3                    :: String -> String -> String -> ST ()
add3 s1 s2 s3           =  S (\(seq, ns) -> ((), (seq |> s1 |> s2 |> s3 |> endl, ns)))

add4                    :: String -> String -> String -> String -> ST ()
add4 s1 s2 s3 s4        =  S (\(seq, ns) -> ((), (seq |> s1 |> s2 |> s3 |> s4 |> endl, ns)))

add5                    :: String -> String -> String -> String -> String -> ST ()
add5 s1 s2 s3 s4 s5     =  S (\(seq, ns) -> ((), (seq |> s1 |> s2 |> s3 |> s4 |> s5 |> endl, ns)))

add6                    :: String -> String -> String -> String -> String -> String -> ST ()
add6 s1 s2 s3 s4 s5 s6  =  S (\(seq, ns) -> ((), (seq |> s1 |> s2 |> s3 |> s4 |> s5 |> s6 |> endl, ns)))
      
addVar                  :: Name -> ST ()
addVar n                =  S (\(seq, ns) -> ((), (seq , n:ns)))     
      
addEndl                 :: ST ()
addEndl                 = add " \n"

comment                 :: String -> ST ()
comment s               =  S (\(seq, ns) -> ((), (seq |> endl, ns)))
--comment s               =  S (\(seq, ns) -> ((), (seq |> "\t;" |> s |> endl, ns)))

commentB                :: String -> ST ()
commentB s               =  S (\(seq, ns) -> ((), (seq |> endl, ns)))
--commentB s              =  S (\(seq, ns) -> ((), (seq |> "\n\t;" |> s |> endl, ns)))

toString                :: ST [String]
toString                = S (\(seq, ns) -> (toList seq, (seq , ns)))


endl                    :: String
endl                    = " \n"

progToFile            :: Prog -> String -> IO()
progToFile p s        = writeFile s $ (intercalate "" (progToARM p))

progToARM             :: Prog -> [String]
progToARM             = codeToARMFull . comp

progToScreen          :: Prog -> IO()
progToScreen          = toScreen . codeToARMFull . comp

codeToARMFull         :: Code -> [String]
codeToARMFull c       = [".data" ++ endl] ++ (addVars c (addMain (codeToARM c))) ++ [endl]

codeToARM             :: Code -> [String]
codeToARM p           = fst $ runState (codeToARM' p) (empty, [])

codeToARM'             :: Code -> ST [String]
codeToARM' []          = toString
codeToARM' (x:xs)      = do instToARM x
                            codeToARM' xs

getRegVal       :: Reg -> String
getRegVal PC    = "pc"
getRegVal SB    = "r12" 
getRegVal LR    = "lr"
getRegVal SP    = "sp"
getRegVal (R n) = n

instToARM                   :: Inst -> ST ()
instToARM (ADDRESS n)       =    addVar n
instToARM (PUSH i)          = do commentB ("PUSH " ++ show i)
                                 add2 "\t mov r3, #" (show i)
                                 add1 "\t push {r3}"
                                 comment "end"
                                 
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
                                 
instToARM (DO op)           = do commentB ("DO " ++ (opToARM op))
                                 add1 "\t pop {r3}"
                                 add1 "\t pop {r4}"
                                 add3 "\t " (opToARM op) " r3, r4, r3" 
                                 add1 "\t push {r3}"
                                 comment "end"
                                 
instToARM (ADD rf r1 imd)   =    add6 "\t add " (getRegVal rf) ", " (getRegVal r1) ", " (getImdVal imd)                        
instToARM (SUB rf r1 imd)   =    add6 "\t sub " (getRegVal rf) ", " (getRegVal r1) ", " (getImdVal imd)
instToARM (MOV r (VAL i))   =    add4 "\t mov " (getRegVal r) ", #" (show i)
instToARM (MOV r (P rs i))  =    add6 "\t add " (getRegVal r) ", " (getRegVal rs) ", #" (show i)
instToARM (B cond l)        =    add4 "\t b" (condToARM cond) " " (getLabel l)
instToARM (BL cond l)       =    add4 "\t bl" (condToARM cond) " " (getLabel l)
instToARM (BX cond r)       =    add4 "\t bx" (condToARM cond) " " (getRegVal r)
instToARM (BXL cond r)      =    add4 "\t bxl" (condToARM cond) " " (getRegVal r)
instToARM (LABEL l)         =    add2 (getLabel l) ":" 

instToARM (PRINT)           = do commentB "PRINT"
                                 add1 "\t pop {r1}" 
                                 add1 "\t ldr r0, addr_of_nr"  
                                 add1 "\t bl printf"
                                 comment "end"
                                 
instToARM (LDR r1 imd)      = case imd of
                                P (G n) _ -> do commentB ("LDR " ++ n ++ " in " ++ (getRegVal r1))
                                                add2 "\t ldr r3, addr_" n 
                                                add3 "\t ldr " (getRegVal r1) ", [r3]"
                                                comment "end"
                                _         ->    add4 "\t ldr " (getRegVal r1) ", " (getImdVal imd)
                                
instToARM (STR r1 imd)      = case imd of
                                P (G n) _ -> do commentB ("STR " ++ n ++ " from " ++ (getRegVal r1))
                                                add2 "\t ldr r3, addr_" n 
                                                add3 "\t str " (getRegVal r1) ", [r3]"
                                                comment "end"
                                _         ->    add4 "\t str " (getRegVal r1) ", " (getImdVal imd)
                                
instToARM (CMPST)           = do commentB "CMPST"
                                 add1 "\t pop {r3}"
                                 add1 "\t pop {r4}"
                                 add1 "\t cmp r3, r4"
                                 comment "end"


getImdVal               :: Imd -> String
getImdVal (VAL i)       = '#':show i
getImdVal (P r i)       = "[" ++ (getRegVal r) ++ ", #" ++ show (i * 4) ++ "]"

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

addMain       :: [String] -> [String]
addMain x     = [".balign 4" ++ endl, "nr: .asciz \"%d \\n\"" ++ endl, ".balign 4" ++ endl,
                      ".text" ++ endl, ".global printf" ++ endl, 
                      ".balign 4" ++ endl, ".global main" ++ endl, {-"main:" ++ endl, 
                      "\t push {lr} " ++ endl,-} endl] 
                      ++ x ++ [endl , {-"\t pop {lr} " ++ endl,
                      "\t bx lr" ++ endl,-} endl, "addr_of_nr : .word nr" ++ endl, endl]

condToARM       :: Cond -> String
condToARM NONE  = ""
condToARM EQ    = "eq"
condToARM NE    = "ne"
condToARM GT    = "gt"
condToARM LT    = "lt"
condToARM GE    = "ge"
condToARM LE    = "le"

                      
addVars                       :: Code -> [String] -> [String]
addVars _                []   = []
addVars []               xs   = xs
addVars ((ADDRESS n):cs) xs   = [".balign 4" ++ endl, "var_" ++ n ++ ": .word 0" ++ endl] ++ (addVars cs xs) ++
                                      ["addr_" ++ n ++ " : .word var_" ++ n ++ endl]
addVars (_:cs)           xs   = addVars cs xs
