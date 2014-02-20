import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr deriving Show
data Unop = Not deriving Show
data Duop = And | Iff deriving Show
data Asgn = Nrm | Add deriving Show
data Stmt = Asg Asgn String Expr | If Expr Stmt [(Expr, Stmt)] (Maybe Stmt) | While Expr Stmt | Seq [Stmt] deriving Show
data Func = Stmt deriving Show

def = javaStyle{ commentStart = "/*"
              , commentEnd = "*/"
              , commentLine = "//"
              , nestedComments = True
              , identStart = letter <|> char '_'
              , identLetter = alphaNum <|> char '_'
              , opStart = oneOf "~&=:"
              , opLetter = oneOf "~&=:"
              , reservedNames = ["true", "false", "if", "then", "else", "while"]
              , reservedOpNames = ["~", "&", "==", "=", "+="] 
              , caseSensitive = True
              }

TokenParser{ parens = m_parens
           , braces = m_braces
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semi = m_semi
           , whiteSpace = m_whiteSpace } = makeTokenParser def

m_semiSep       :: Parser a -> Parser [a]
m_semiSep stmt  = sepEndBy1 stmt (optional m_semi)

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "==" >> return (Duo Iff)) AssocLeft]
        ]
term = m_parens exprparser
       <|> fmap Var m_identifier
       <|> (m_reserved "true" >> return (Con True))
       <|> (m_reserved "false" >> return (Con False))

asgnParser   :: String -> Parser Stmt
asgnParser v =     do { m_reservedOp "="
                      ; e <- exprparser
                      ; return (Asg Nrm v e)
                      }
               <|> do { m_reservedOp "+="
                      ; e <- exprparser
                      ; return (Asg Add v e)
                      }

mainparser :: Parser Stmt
mainparser = m_whiteSpace >> stmtparser <* eof
    where
      stmtparser :: Parser Stmt
      stmtparser = fmap Seq (m_semiSep stmt1)
      stmt1 = 
                  do { v <- m_identifier
                     ; asgnParser v
                     }
              <|> do { m_reserved "if"
                     ; b <- m_parens exprparser
                     ; p <- m_braces stmtparser
                     ; m_reserved "else"
                     ; q <- m_braces stmtparser
                     ; return (If b p [] (Just q))
                     }
              <|> do { m_reserved "while"
                     ; b <- m_parens exprparser
                     ; p <- m_braces stmtparser
                     ; return (While b p)
                     }

test :: String -> IO ()
test inp = case parse mainparser "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }
             
testFile            :: String -> IO ()
testFile fileName = do file <- readFile fileName
                       test file        
