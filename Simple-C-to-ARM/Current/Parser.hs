module Parser (
     parseFile
) where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Pos
import Prelude hiding (EQ, LT, GT)
import AST

types :: [String]
types = ["int", "void"]

conditional :: [String]
conditional = ["if", "else", "while", "for"]

comparators :: [String]
comparators = ["==", "!=", "<", "<=", ">", ">="]

def = javaStyle{ commentStart = "/*"
              , commentEnd = "*/"
              , commentLine = "//"
              , nestedComments = True
              , identStart = letter <|> char '_'
              , identLetter = alphaNum <|> char '_'
              , opStart = oneOf "~&=:"
              , opLetter = oneOf "~&=:"
              , reservedNames = ["true", "false", "print", "return"] ++ types ++ conditional
              , reservedOpNames = ["~", "&", "=", "+=", "+", "-", "*"] ++ comparators 
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
        ,-} [Infix ( do {pos <- getPosition  ; m_reservedOp "*" ; return (App pos Mul)}) AssocLeft]
        , [Infix (do {pos <- getPosition ; m_reservedOp "+" ; return (App pos Add)}) AssocLeft]
        , [Infix (do {pos <- getPosition ; m_reservedOp "-" ; return (App pos Sub)}) AssocLeft]
        , [Infix (do {pos <- getPosition ; m_reservedOp "==" ; return (Compare pos EQ)}) AssocNone]
        , [Infix (do {pos <- getPosition ; m_reservedOp "!=" ; return (Compare pos NE)}) AssocNone]
        , [Infix (do {pos <- getPosition ; m_reservedOp "<"  ; return (Compare pos LT)}) AssocNone]
        , [Infix (do {pos <- getPosition ; m_reservedOp "<=" ; return (Compare pos LE)}) AssocNone]
        , [Infix (do {pos <- getPosition ; m_reservedOp ">"  ; return (Compare pos GT)}) AssocNone]
        , [Infix (do {pos <- getPosition ; m_reservedOp ">=" ; return (Compare pos GE)}) AssocNone]
        ]
term = m_parens exprParser
       <|> do { pos <- getPosition
              ; try $ funcParserExpr pos
              }
       <|> do { pos <- getPosition
              ; fmap (Var pos) m_identifier
              }
       <|> do { pos <- getPosition
              ; fmap (Val pos) m_integer
              }
       <|> do { pos <- getPosition
              ; m_reserved "true"
              ; return (Val pos 1)
              }
       <|> do { pos <- getPosition
              ; m_reserved "false" 
              ; return (Val pos 0)
              }
       
 
asgnParser              :: String -> SourcePos -> Parser Stmt
asgnParser v pos        = do { m_reservedOp "="
                             ; e <- exprParser
                             ; return (Assign pos v e)
                             }
                    {-<|> do { m_reservedOp "+="
                             ; e <- exprparser
                             ; return (Asg Add1 v e)
                             }-}

funcParser              :: String -> SourcePos -> Parser Stmt
funcParser v pos        = do { e <- m_parens ( m_commaSep exprParser )
                             ; return (Ex (Apply pos v e))
                             }

funcParserExpr          :: SourcePos -> Parser Expr
funcParserExpr pos      = do { v <- m_identifier
                             ; e <- m_parens ( m_commaSep exprParser )
                             ; return (Apply pos v e)
                             }
                     
forParser    :: Parser (String, Stmt, Expr, Stmt)
forParser    =  do { pos <- getPosition
                   ; m_reserved "int"
                   ; d <- m_identifier
                   ; a <- asgnParser d pos
                   ; m_semi
                   ; e <- exprParser
                   ; m_semi
                   ; i <- stmtParser
                   ; return (d, a, e, i)
                   }
                     
stmtParser :: Parser Stmt
stmtParser = fmap Seqn (m_semiSep stmt1)
    where
      stmt1 =     do { m_reserved "int"
                     ; v <- m_identifier
                     ; return (LocalVar v)
                     }
              <|> do { pos <- getPosition
                     ; v <- m_identifier
                     ; choice [ try (asgnParser v pos) , try (funcParser v pos) ]
                     }
              <|> do { m_reserved "if"
                     ; b <- m_parens exprParser
                     ; p <- m_braces stmtParser
                     ; do { m_reserved "else"
                          ; q <- m_braces stmtParser
                          ; return (If b (SeqnE [p]) (SeqnE [q]))
                          }
                       <|> return (If b p (Seqn []))
                     }
              <|> do { m_reserved "while"
                     ; b <- m_parens exprParser
                     ; p <- m_braces stmtParser
                     ; return (While b (SeqnE [p]))
                     }
              <|> do { m_reserved "for"
                     ; (d, a, e, i) <- m_parens forParser
                     ; p <- m_braces stmtParser
                     ; return (SeqnE [LocalVar d, a, While e (SeqnE [p, i])])
                     }
              <|> do { m_reserved "print"
                     ; b <- m_parens exprParser
                     ; return (Print b)
                     }
              <|> do { m_reserved "return"
                     ; e <- m_parens exprParser
                     ; return (Return e)
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
      decl =      do { pos <- getPosition
                     ; m_reserved "int"
                     ; v <- m_identifier
                     ; m_semi
                     ; return (GlobalVar v pos)
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
             ; Right ans -> do { print "parse successful"
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
