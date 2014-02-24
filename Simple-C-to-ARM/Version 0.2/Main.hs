
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
import SampleProg
import ASTCompiler
import VMInst
import System.Environment (getArgs)

compile            :: String -> IO()
compile fileName   = do prog <- parseFile fileName
                        print "compilation successful"
                        progToFile prog
                        print "code generation successful"

compileToScreen          :: String -> IO()
compileToScreen fileName = do prog <- parseFile fileName
                              progToScreen prog

runInVM           :: String -> IO()
runInVM fileName  = do prog <- parseFile fileName
                       print ( execM (comp prog) )

printInst           :: String -> IO()
printInst fileName  = do prog <- parseFile fileName
                         print (comp prog)
                         
main = do args <- getArgs
          case args of 
                []      -> print "no file as argument"
                (x:xs)  -> compile x