
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
import Helper
import System.Environment (getArgs)
import System.Exit
import Analyser

compile                      :: String -> String -> IO()
compile fileName outputFile  = do code <- getCode fileName
                                  case code of
                                        [] -> exitFailure
                                        _  -> do codeToFile  code outputFile
                                                 exitSuccess
                                  
compileToScreen          :: String -> IO()
compileToScreen fileName = do code <- getCode fileName
                              case code of
                                        [] -> putStr "failed to compile\n"
                                        _  -> codeToScreen code

runInVM           :: String -> IO()
runInVM fileName  = do code <- getCode fileName
                       case code of
                                [] -> putStr "failed to compile\n"
                                _  -> execPrint code

getCode           :: String -> IO Code
getCode fileName  = do prog <- parseFile fileName
                       prog' <- compE prog
                       code <- compI $ cleanIProg prog'
                       return code

printInst           :: String -> IO()
printInst fileName  = do code <- getCode fileName
                         print code

printIProg           :: String -> IO()
printIProg fileName  = do prog <- parseFile fileName
                          prog' <- compE prog
                          print prog'
                          putStr "\n--------------------------\n"
                          let prog'' = cleanIProg prog'
                          print $ prog''
                          putStr "\n--------------------------\n"
                          ppIProg $ analyse prog''
                          
main = do args <- getArgs
          case args of 
              []        -> putStr "no file as argument"
              [x]       -> compile x "out.s"
              (x:y:xs)  -> compile x y