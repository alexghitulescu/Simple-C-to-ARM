
module CodeGen (
     progToFile,
     progToScreen
) where

import AST
import VMInst
import ASTCompiler

endl                        :: String
endl                        = " \n"

progToFile            :: Prog -> IO()
progToFile p          = writeFile "out.s" $ unwords (progToARM p)

progToARM             :: Prog -> [String]
progToARM             = codeToARMFull . comp

progToScreen          :: Prog -> IO()
progToScreen          = toScreen . codeToARMFull . comp

codeToARMFull         :: Code -> [String]
codeToARMFull c       = [".data" ++ endl] ++ (addVars c (addMain (codeToARM c))) ++ [endl]

codeToARM             :: Code -> [String]
codeToARM []          = []
codeToARM (x:xs)      = (instToARM x) ++ (codeToARM xs)

instToARM                   :: Inst -> [String]
instToARM (ADDRESS n)       = []
instToARM (PUSH i)          = ["\t mov r1, #" ++ show(i) ++ endl, "\t push {r1}" ++ endl]
instToARM (PUSHV n)         = ["\t ldr r1, addr_" ++ n ++ endl, "\t ldr r1, [r1]" ++ endl, "\t push {r1}" ++ endl]
instToARM (POP n)           = ["\t ldr r1, addr_" ++ n ++ endl, "\t pop {r2}" ++ endl, "\t str r2, [r1]" ++ endl]
instToARM (DO op)           = ["\t pop {r1}" ++ endl, "\t pop {r2}" ++ endl,
                                              "\t " ++ opToARM(op) ++ " r1, r2, r1" ++ endl, "\t push {r1}" ++ endl]
instToARM (JUMP l)          = ["\t b label" ++ show(l) ++ endl]
instToARM (JUMPZ l)         = ["\t pop {r1}" ++ endl, "\t cmp r1, #0" ++ endl, "\t beq label" ++ show(l) ++ endl]
instToARM (LABEL l)         = ["label" ++ show(l) ++ ":" ++ endl]
instToARM (PRINT)           = ["\t pop {r1}" ++ endl, "\t ldr r0, addr_of_nr" ++ endl, "\t bl printf" ++ endl]

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
                      "return: .word 0" ++ endl, ".text" ++ endl, ".global printf" ++ endl, 
                      ".balign 4" ++ endl, ".global main" ++ endl, "main:" ++ endl, 
                      "\t ldr r1, addr_of_return" ++ endl, "\t str lr, [r1]" ++ endl, endl] 
                      ++ x ++ [endl ,"\t ldr lr, addr_of_return" ++ endl, "\t ldr lr, [lr]" 
                      ++ endl, "\t bx lr" ++ endl, endl, "addr_of_nr : .word nr" ++ endl,
                      "addr_of_return : .word return" ++ endl, endl]

addVars                       :: Code -> [String] -> [String]
addVars _                []   = []
addVars []               xs   = xs
addVars ((ADDRESS n):cs) xs   = [".balign 4" ++ endl, "var_" ++ n ++ ": .word 0" ++ endl] ++ (addVars cs xs) ++
                                      ["addr_" ++ n ++ " : .word var_" ++ n ++ endl]
addVars (_:cs)           xs   = addVars cs xs
