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

types :: [String]
types = ["int", "void"]

conditional :: [String]
conditional = ["if", "else", "while"]

def = javaStyle{ commentStart = "/*"
              , commentEnd = "*/"
              , commentLine = "//"
              , nestedComments = True
              , identStart = letter <|> char '_'
              , identLetter = alphaNum <|> char '_'
              , opStart = oneOf "~&=:"
              , opLetter = oneOf "~&=:"
              , reservedNames = ["true", "false", "print"] ++ types ++ conditional
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
           , commaSep = m_commaSep
           , whiteSpace = m_whiteSpace } = makeTokenParser def

m_semiSep       :: Parser a -> Parser [a]
m_semiSep stmt  = sepEndBy1 stmt (optional m_semi)

exprParser :: Parser Expr
exprParser = buildExpressionParser table term <?> "expression"
table = [ {-[Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "==" >> return (Duo Iff)) AssocLeft]
        ,-} [Infix (m_reservedOp "+" >> return (App Add)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (App Sub)) AssocLeft]
        , [Infix (m_reservedOp "*" >> return (App Mul)) AssocLeft]
        ]
term = m_parens exprParser
       <|> fmap Var m_identifier
       <|> fmap Val m_integer
       <|> (m_reserved "true" >> return (Val 1))
       <|> (m_reserved "false" >> return (Val 0))
 
asgnParser   :: String -> Parser Stmt
asgnParser v =     do { m_reservedOp "="
                      ; e <- exprParser
                      ; return (Assign v e)
                      }
               {-<|> do { m_reservedOp "+="
                      ; e <- exprparser
                      ; return (Asg Add1 v e)
                      }-}

funcParser   :: String -> Parser Stmt
funcParser v =    do { e <- m_parens ( m_commaSep exprParser )
                     ; return (Apply v e)
                     }
                      
stmtParser :: Parser Stmt
stmtParser = fmap Seqn (m_semiSep stmt1)
    where
      stmt1 =     do { m_reserved "int"
                     ; v <- m_identifier
                     ; return (LocalVar v)
                     }
              <|> do { v <- m_identifier
                     ; choice [ try (asgnParser v) , try (funcParser v) ]
                     }
              <|> do { m_reserved "if"
                     ; b <- m_parens exprParser
                     ; p <- m_braces stmtParser
                     ; m_reserved "else"
                     ; q <- m_braces stmtParser
                     ; return (If b p q)
                     }
              <|> do { m_reserved "while"
                     ; b <- m_parens exprParser
                     ; p <- m_braces stmtParser
                     ; return (While b p)
                     }
              <|> do { m_reserved "print"
                     ; b <- m_parens exprParser
                     ; return (Print b)
                     }
                  

                  
mainParser :: Parser Prog
mainParser = m_whiteSpace >> progParser <* eof
    where
      progParser :: Parser Prog
      progParser = fmap PSeq (m_semiSep prog1)
      prog1 =     do { try func
                     }
              <|> do { try decl
                     }
      decl =      do { m_reserved "int"
                     ; v <- m_identifier
                     ; m_semi
                     ; return (GlobalVar v)
                     }
      func =      do { m_reserved "int"
                     ; v <- m_identifier
                     ; e <- m_parens ( m_commaSep args )
                     ; p <- m_braces stmtParser
                     ; return (Fun v e p)
                     }
      args =      do { m_reserved "int"
                     ; v <- m_identifier
                     ; return (v)
                     }
      
      
parseF :: String -> IO (Prog)
parseF inp = case parse mainParser "" inp of
             { Left err ->  do { print err
                               ; return (PSeq [])
                               }
             ; Right ans -> do { print "parse successful\n"
                               ; return ans
                               }
             }

parseFile :: String -> IO (Prog)
parseFile fileName = do file <- readFile fileName
                        parseF file  

test :: String -> IO ()
test inp = case parse mainParser "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }

testFile            :: String -> IO ()
testFile fileName = do file <- readFile fileName
                       test file        