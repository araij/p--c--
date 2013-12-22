module P_CParser where

import Control.Monad (liftM, void)
import Control.Applicative ((<$))
import Text.Parsec (manyTill, between, lookAhead, anyChar, string, (<|>), parse, try, many, spaces,
                    eof)
import Text.Parsec.String (Parser)
import Text.Parsec.Error (ParseError)
import ParserUtil

-- p_c_program ::= (statement)+
-- statement   ::= if_statement
--               | while_statement
--               | repeat_statement
-- if_statement ::= if expression then (statement)+ else (statement)+ end
--                | if expression then (statement)+ end
-- while_statement ::= while expression do (statement)+ end
-- repeat_statement ::= repeat (statement)+ until expression
-- if         ::= "\n\s*if\s+"
-- then       ::= "\s+then\s*\n"
-- while      ::= "\n\s*while\s+"
-- repeat     ::= "\n\s*repeat\s*\n"
-- until      ::= "\n\s*until\s+"
-- do         ::= "\s+do\s*\n"
-- end        ::= "\n\s*end\s*\n"
-- expression ::= ("([^\n])\\\n\s*" ==> \1) ++ expression
--              | ("([^\n])\\\n" ==> \1)

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
    <|> pProc

pIfThen :: Parser Stmt
pIfThen = do
  cond <- between pIf pThen pExp
  yes  <- manyTill pStat $ lookAhead (try pElse <|> try pEnd)
  (SIfThen cond yes <$ try pEnd) <|> (SIfThenElse cond yes `liftM` elseClause)
  where
    elseClause = pElse >> manyTill pStat (try pEnd)

pWhileDo :: Parser Stmt
pWhileDo = do
  cond <- between pWhile pDo pExp
  ss   <- manyTill pStat $ try pEnd
  return $ SWhile cond ss

pRepeatUntil :: Parser Stmt
pRepeatUntil = do
  pRepeat
  ss   <- manyTill pStat $ try pUntil
  cond <- pExp
  return $ SRepeat ss cond

pProc :: Parser Stmt
pProc = spaces >> (SProc `liftM` pExp)

pExp :: Parser String
pExp = manyTill1 anyChar (lookAhead (try expEnd))
  where
    expEnd = try pThen <|> try pDo <|> try (spacesLn >> eol) <|> (spacesLn >> eof)

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

----
---- from http://d.hatena.ne.jp/kazu-yamamoto/20110131/1296466529
----
--appear :: Parser a -> Parser [a]
--appear p = foldr ($) [] <$> many ((:) <$> try p <|> flip const <$> anyChar)

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

