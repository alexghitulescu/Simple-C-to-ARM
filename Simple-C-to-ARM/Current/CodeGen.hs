
module CodeGen (
     progToFile,
     progToScreen
) where

import Prelude hiding (EQ, LT, GT)
import AST
import VMInst
import ASTCompiler

endl                        :: String
endl                        = " \n"

progToFile            :: Prog -> String -> IO()
progToFile p s        = writeFile s $ unwords (progToARM p)

progToARM             :: Prog -> [String]
progToARM             = codeToARMFull . comp

progToScreen          :: Prog -> IO()
progToScreen          = toScreen . codeToARMFull . comp

codeToARMFull         :: Code -> [String]
codeToARMFull c       = [".data" ++ endl] ++ (addVars c (addMain (codeToARM c))) ++ [endl]

codeToARM             :: Code -> [String]
codeToARM []          = []
codeToARM (x:xs)      = (instToARM x) ++ (codeToARM xs)

getRegVal       :: Reg -> String
getRegVal PC    = "pc"
getRegVal SB    = "r12" 
getRegVal LR    = "lr"
getRegVal SP    = "sp"
getRegVal (R n) = n

instToARM                   :: Inst -> [String]
instToARM (ADDRESS n)       = []
instToARM (PUSH i)          = ["\t mov r3, #" ++ show(i) ++ endl, "\t push {r3}" ++ endl]
instToARM (PUSHV (G n))     = ["\t ldr r3, addr_" ++ n ++ endl, "\t ldr r3, [r3]" ++ endl, "\t push {r3}" ++ endl]
instToARM (PUSHV r)         = ["\t push {" ++ (getRegVal r) ++ "}" ++ endl]
instToARM (POP (G n))       = ["\t ldr r3, addr_" ++ n ++ endl, "\t pop {r4}" ++ endl, "\t str r4, [r3]" ++ endl]
instToARM (POP r)           = ["\t pop {" ++ (getRegVal r) ++ "}" ++ endl]
instToARM (DO op)           = ["\t pop {r3}" ++ endl, "\t pop {r4}" ++ endl,
                                              "\t " ++ opToARM(op) ++ " r3, r4, r3" ++ endl, "\t push {r3}" ++ endl]
instToARM (ADD rf r1 imd)   = ["\t add " ++ (getRegVal rf) ++ ", " ++ (getRegVal r1) ++ ", " ++ (getImdVal imd) ++ endl]
instToARM (SUB rf r1 imd)   = ["\t sub " ++ (getRegVal rf) ++ ", " ++ (getRegVal r1) ++ ", " ++ (getImdVal imd) ++ endl]
instToARM (MOV r (VAL i))   = ["\t mov " ++ (getRegVal r) ++ ", #" ++ show i ++ endl]
instToARM (MOV r (P rs i))  = ["\t add " ++ (getRegVal r) ++ ", " ++ (getRegVal rs) ++ ", #" ++ show i ++ endl]
instToARM (B cond l)        = ["\t b" ++ (condToARM cond) ++ " " ++ getLabel l ++ endl]
instToARM (BL cond l)       = ["\t bl" ++ (condToARM cond) ++ " " ++ getLabel l ++ endl]
instToARM (BX cond r)       = ["\t bx" ++ (condToARM cond) ++ " " ++ getRegVal r ++ endl]
instToARM (BXL cond r)      = ["\t bxl" ++ (condToARM cond) ++ " " ++ getRegVal r ++ endl]
instToARM (LABEL l)         = [getLabel l ++ ":" ++ endl]
instToARM (PRINT)           = ["\t pop {r1}" ++ endl, "\t ldr r0, addr_of_nr" ++ endl, "\t bl printf" ++ endl]
instToARM (LDR r1 imd)      = case imd of
                                P (G n) _ -> ["\t ldr " ++ getRegVal r1 ++ ", addr_" ++ n ++ endl, "\t ldr r3, [r3]" ++ endl]
                                _         -> ["\t ldr " ++ getRegVal r1 ++ ", " ++ getImdVal imd ++ endl]
instToARM (STR r1 imd)      = case imd of
                                P (G n) _ -> ["\t ldr r3, addr_" ++ n ++ endl, "\t str " ++ getRegVal r1 ++ ", [r3]" ++ endl]
                                _         -> ["\t str " ++ getRegVal r1 ++ ", " ++ getImdVal imd ++ endl]
instToARM (CMPST)           = ["\t pop {r3}" ++ endl, "\t pop {r4}" ++ endl, "\t cmp r3, r4" ++ endl]


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
