
module Main (
     compile,
     runInVM,
     printInst,
     main
) where

import AST
import Parser
import CodeGen
import VMRunner
--import SampleProg
import ASTCompiler
import IASTCompiler
import VMInst
import System.Environment (getArgs)

compile                      :: String -> String -> IO()
compile fileName outputFile  = do prog <- parseFile fileName
                                  putStr "compilation successful\n"
                                  prog' <- compE prog
                                  progToFile  prog' outputFile
                                  putStr "code generation successful\n"

compileToScreen          :: String -> IO()
compileToScreen fileName = do prog <- parseFile fileName
                              prog' <- compE prog
                              progToScreen prog'

runInVM           :: String -> IO()
runInVM fileName  = do prog <- parseFile fileName
                       prog' <- compE prog
                       code <- compI prog'
                       execPrint code

printInst           :: String -> IO()
printInst fileName  = do prog <- parseFile fileName
                         prog' <- compE prog 
                         code <- compI prog'
                         print code

printIProg           :: String -> IO()
printIProg fileName  = do prog <- parseFile fileName
                          prog' <- compE prog
                          print prog'
                          
main = do args <- getArgs
          case args of 
              []        -> putStr "no file as argument"
              [x]       -> compile x "out.s"
              (x:y:xs)  -> compile x y