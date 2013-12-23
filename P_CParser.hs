module P_CParser where

import Control.Monad (liftM, void)
import Control.Applicative ((<$), (<$>))
import Text.Parsec (manyTill, lookAhead, anyChar, string, (<|>), parse, try, many, spaces, eof)
import Text.Parsec.String (Parser)
import Text.Parsec.Error (ParseError)
import ParserUtil

-- p_c_program ::= (<statement>)+
-- statement   ::= <if_statement>
--               | <while_statement>
--               | <expression>
-- if_statement ::= if <expression> then (<statement>)+ else (<statement>)+ end
--                | if <expression> then (<statement>)+ end
-- while_statement ::= while <expression> do (<statement>)+ end

data P_CProg = P_CProg [Stmt]
  deriving (Show, Eq)

data Stmt = SIfThen String [Stmt]
          | SIfThenElse String [Stmt] [Stmt]
	  | SWhile String [Stmt]
	  | SProc String
  deriving (Show, Eq)

parseP_C :: String -> Either ParseError P_CProg
parseP_C = parse pP_CProg "P--C-- Program" . ('\n' :)

pP_CProg :: Parser P_CProg
pP_CProg = do
  ss <- many (try pStmt)
  spaces
  eof
  return $ P_CProg ss

pStmt :: Parser Stmt
pStmt = try pIfThen
    <|> try pWhileDo
    <|> pProc

pIfThen :: Parser Stmt
pIfThen = do
  pIf
  cond <- pExpTo pThen
  yes  <- manyTill pStmt $ lookAhead (try pElse <|> try pEnd)
  (SIfThen cond yes <$ try pEnd) <|> (SIfThenElse cond yes <$> elseClause)
  where
    elseClause = pElse >> manyTill pStmt (try pEnd)

pWhileDo :: Parser Stmt
pWhileDo = do
  pWhile
  cond <- pExpTo pDo
  ss   <- manyTill pStmt $ try pEnd
  return $ SWhile cond ss

pProc :: Parser Stmt
pProc = SProc <$> pExpTo (lookAhead (try trail))

--
-- an expression ends with `end`
-- `end` is consumed
--
pExpTo :: Parser a -> Parser String
pExpTo end = do
  spaces
  -- `hd` ends with appearance of `end` or newline
  hd <- manyTill1 anyChar (lookAhead (void (try end) <|> try trail))
  -- `tl` is empty if `hd` was ended with `end`
  -- `tl` is an expression in following lines if `hd` was ended with newline
  tl <- (try end >> return "") <|> (trail >> liftM ('\n' :) (pExpTo end))
  return $ hd ++ tl

--
-- matches "\n\s+<string>\s+"
--
lineHead :: String -> Parser String
lineHead t = do
  eol
  spaces
  s <- string t
  lookAhead (space1 <|> eof)
  return s

--
-- matches "\s+<string>\s*\n"
--
lineTail :: String -> Parser String
lineTail t = do
  space1
  s <- string t
  spacesLn
  lookAhead (eol <|> eof)
  return s

pIf :: Parser ()
pIf = void $ lineHead "if"

pThen :: Parser ()
pThen = void $ lineTail "then"

pElse :: Parser ()
pElse = void $ lineHead "else"

pWhile :: Parser ()
pWhile = void $ lineHead "while"

pUntil :: Parser ()
pUntil = void $ lineHead "until"

pDo :: Parser ()
pDo = void $ lineTail "do"

pEnd :: Parser ()
pEnd = void $ lineHead "end"

trail :: Parser ()
trail = spacesLn >> (eof <|> eol)

