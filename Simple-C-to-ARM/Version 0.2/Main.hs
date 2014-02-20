
module Main (
     compile
) where

import AST
import Parser
import CodeGen
import VMRunner
import SampleProg
import ASTCompiler

compile            :: String -> IO()
compile fileName   = do prog <- parseFile fileName
                        print "compilation successful\n"
                        progToFile prog
                        print "code generation successful\n"

compileToScreen          :: String -> IO()
compileToScreen fileName = do prog <- parseFile fileName
                              progToScreen prog

runInVM           :: String -> IO()
runInVM fileName  = do prog <- parseFile fileName
                       print ( exec (comp prog) )
