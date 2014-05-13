
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
                                _  -> execPrint code True

getCode           :: String -> IO Code
getCode fileName  = do prog <- parseFile fileName
                       prog' <- compE prog
                       let prog'' = cleanIProg prog'
                       code <- compI $ analyse prog''
                       return code

printInst           :: String -> IO()
printInst fileName  = do code <- getCode fileName
                         mapM_ (putStrLn.show) code

printIProg           :: String -> IO()
printIProg fileName  = do prog <- parseFile fileName
                          prog' <- compE prog
                          ppIProg prog'
                          putStr "\n--------------------------\n"
                          let prog'' = cleanIProg prog'
                          ppIProg $ prog''
                          putStr "\n--------------------------\n"
                          ppIProg $ analyse prog''
                          
main = do args <- getArgs
          let prc = parseArgs def args
          case prc of 
                A _ _ True _    -> do putStr "output flag with no output file"
                                      exitFailure
                A f1 _ _ True   -> runInVM f1
                A f1 f2 _ _     -> compile f1 f2
              
data Arg = A String String Bool Bool
def = A "no file" "out.s" False False

parseArgs                               :: Arg -> [String] -> Arg
parseArgs args             []           = args
parseArgs (A f1 _ True vm) (x:xs)       = parseArgs (A f1 x False vm) xs
parseArgs (A f1 f2 _ vm) ("-o":xs)      = parseArgs (A f1 f2 True vm) xs
parseArgs (A f1 f2 out _) ("-vm":xs)    = parseArgs (A f1 f2 out True) xs
parseArgs (A _ f2 out vm) (x:xs)        = parseArgs (A x f2 out vm) xs