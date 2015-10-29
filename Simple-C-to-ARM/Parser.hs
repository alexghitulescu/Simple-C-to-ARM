module Parser (
     parseFile
) where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import AST

--data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr deriving Show
--data Unop = Not deriving Show
--data Duop = And | Iff | Add | Sub | Mul deriving Show
--data Asgn = Nrm | Add1 deriving Show
--data Stmt = Asg Asgn String Expr | If Expr Stmt [(Expr, Stmt)] (Maybe Stmt) | While Expr Stmt | Seq [Stmt] deriving Show
--data Func = Stmt deriving Show

def = javaStyle{ commentStart = "/*"
              , commentEnd = "*/"
              , commentLine = "//"
              , nestedComments = True
              , identStart = letter <|> char '_'
              , identLetter = alphaNum <|> char '_'
              , opStart = oneOf "~&=:"
              , opLetter = oneOf "~&=:"
              , reservedNames = ["true", "false", "if", "else", "while", "int", "print"]
              , reservedOpNames = ["~", "&", "==", "=", "+=", "+", "-", "*"] 
              , caseSensitive = True
              }

TokenParser{ parens = m_parens
           , braces = m_braces
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , integer = m_integer
           , semi = m_semi
           , whiteSpace = m_whiteSpace } = makeTokenParser def

m_semiSep       :: Parser a -> Parser [a]
m_semiSep stmt  = sepEndBy1 stmt (optional m_semi)

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ {-[Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "==" >> return (Duo Iff)) AssocLeft]
        ,-} [Infix (m_reservedOp "+" >> return (App Add)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (App Sub)) AssocLeft]
        , [Infix (m_reservedOp "*" >> return (App Mul)) AssocLeft]
        ]
term = m_parens exprparser
       <|> fmap Var m_identifier
       <|> (fmap Val m_integer)
       <|> (m_reserved "true" >> return (Val 1))
       <|> (m_reserved "false" >> return (Val 0))

asgnParser   :: String -> Parser Prog
asgnParser v =     do { m_reservedOp "="
                      ; e <- exprparser
                      ; return (Assign v e)
                      }
               {-<|> do { m_reservedOp "+="
                      ; e <- exprparser
                      ; return (Asg Add1 v e)
                      }-}

mainparser :: Parser Prog
mainparser = m_whiteSpace >> stmtparser <* eof
    where
      stmtparser :: Parser Prog
      stmtparser = fmap Seqn (m_semiSep stmt1)
      stmt1 =     do { m_reserved "int"
                     ; v <- m_identifier
                     ; return (NewVar v)
                     }
              <|> do { v <- m_identifier
                     ; asgnParser v
                     }
              <|> do { m_reserved "if"
                     ; b <- m_parens exprparser
                     ; p <- m_braces stmtparser
                     ; m_reserved "else"
                     ; q <- m_braces stmtparser
                     ; return (If b p q)
                     }
              <|> do { m_reserved "while"
                     ; b <- m_parens exprparser
                     ; p <- m_braces stmtparser
                     ; return (While b p)
                     }
              <|> do { m_reserved "print"
                     ; b <- m_parens exprparser
                     ; return (Print b)
                     }
parseF :: String -> IO (Prog)
parseF inp = case parse mainparser "" inp of
             { Left err ->  do { print err
                               ; return (Seqn [])
                               }
             ; Right ans -> do { print "parse successful\n"
                               ; return ans
                               }
             }

parseFile :: String -> IO (Prog)
parseFile fileName = do file <- readFile fileName
                        parseF file  

test :: String -> IO ()
test inp = case parse mainparser "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }

testFile            :: String -> IO ()
testFile fileName = do file <- readFile fileName
                       test file        
