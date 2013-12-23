module P_CParser where

import Control.Monad (liftM, void)
import Control.Applicative ((<$))
import Text.Parsec (manyTill, between, lookAhead, anyChar, string, (<|>), parse, try, many, spaces,
                    eof)
import Text.Parsec.String (Parser)
import Text.Parsec.Error (ParseError)
import ParserUtil

-- p_c_program ::= (<statement>)+
-- statement   ::= <if_statement>
--               | <while_statement>
--               | <repeat_statement>
--               | <expression>
-- if_statement ::= if <expression> then (<statement>)+ else (<statement>)+ end
--                | if <expression> then (<statement>)+ end
-- while_statement ::= while <expression> do (<statement>)+ end
-- repeat_statement ::= repeat (<statement>)+ until <expression>

data P_CProg = P_CProg [Stmt]
  deriving (Show, Eq)

data Stmt = SIfThen String [Stmt]
          | SIfThenElse String [Stmt] [Stmt]
	  | SWhile String [Stmt]
	  | SRepeat [Stmt] String
	  | SProc String
  deriving (Show, Eq)

parseP_C :: String -> Either ParseError P_CProg
parseP_C = parse pP_CProg "P--C-- Program" . ('\n' :)

pP_CProg :: Parser P_CProg
pP_CProg = do
  ss <- many (try pStat)
  spaces
  eof
  return $ P_CProg ss

pStat :: Parser Stmt
pStat = try pIfThen
    <|> try pWhileDo
    <|> try pRepeatUntil
    <|> SProc `liftM` (pExp trail)

pIfThen :: Parser Stmt
pIfThen = do
  cond <- between pIf pThen $ pExp pThen
  yes  <- manyTill pStat $ lookAhead (try pElse <|> try pEnd)
  (SIfThen cond yes <$ try pEnd) <|> (SIfThenElse cond yes `liftM` elseClause)
  where
    elseClause = pElse >> manyTill pStat (try pEnd)

pWhileDo :: Parser Stmt
pWhileDo = do
  cond <- between pWhile pDo $ pExp pDo
  ss   <- manyTill pStat $ try pEnd
  return $ SWhile cond ss

pRepeatUntil :: Parser Stmt
pRepeatUntil = do
  pRepeat
  ss   <- manyTill pStat $ try pUntil
  cond <- pExp trail
  return $ SRepeat ss cond

trail :: Parser ()
trail = spacesLn >> (eof <|> eol)

pExp :: Parser a -> Parser String
pExp end = spaces >> manyTill1 anyChar (lookAhead (try end))

--
-- matches "\n\s+<string>\s+"
--
lineHead :: String -> Parser String
lineHead t = do
  eol
  spaces
  s <- string t
  space1 <|> eof
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

pRepeat :: Parser ()
pRepeat = void $ lineHead "repeat"

pUntil :: Parser ()
pUntil = void $ lineHead "until"

pDo :: Parser ()
pDo = void $ lineTail "do"

pEnd :: Parser ()
pEnd = void $ lineHead "end"

