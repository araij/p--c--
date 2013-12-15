module Upc where

import Control.Applicative ((<$>), (*>), (<*), pure)
import Control.Monad (liftM, void)
import Text.Parsec (char, string, noneOf, anyChar, (<|>), parse, try, many, space, spaces,
                    skipMany, eof, ParsecT, endBy)
import Text.Parsec.String (Parser)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Combinator (sepBy)

data Upc = Upc [Step]
  deriving (Show)

data Step = Step (Maybe String) Statement
  deriving (Show)

data Statement = Statement (Maybe String) (Maybe String) String
  deriving (Show)

-- upc_source          ::= line "\n" (upc_source | ε)
-- line                ::= step eol
-- step                ::= branch_statement | label':' statement | statement
-- statement           ::= branch_statement | action
-- branch_statement    ::= if_statement | unless_statement | whichever_statement
-- if_statement        ::= "__GOTO_IF__" label ":" action
-- unless_statement    ::= "__GOTO_UNLESS__" label ":" action
-- whichever_statement ::= "__GOTO__" label ":" label ":" action
-- label               ::= [^:\r\n]+

parseUpc :: String -> Either ParseError Upc
parseUpc = parse upcSrc "Unstructured P--C--"

upcSrc :: Parser Upc
upcSrc = do
  spaces
  steps <- step `endBy` spaces
  eof
  return $ Upc steps

step :: Parser Step
step = try (Step Nothing `liftM` branchStatement)
   <|> try labeledStep
   <|> Step Nothing `liftM` statement

labeledStep :: Parser Step
labeledStep = do
  l <- label
  char ':'
  spaces
  s <- statement
  return $ Step (Just l) s

statement :: Parser Statement
statement = try branchStatement
        <|> fallStatement

branchStatement :: Parser Statement
branchStatement = try ifStatement
              <|> try unlessStatement
	      <|> whicheverStatement

ifStatement :: Parser Statement
ifStatement = do
  string "__GOTO_IF__"
  space1
  yes <- label
  char ':'
  spaces
  act <- action
  return $ Statement (Just yes) Nothing act

unlessStatement :: Parser Statement
unlessStatement = do
  string "__GOTO_UNLESS__"
  space1
  no <- label
  char ':'
  spaces
  act <- action
  return $ Statement Nothing (Just no) act

whicheverStatement :: Parser Statement
whicheverStatement = do
  string "__GOTO__"
  space1
  yes <- label
  char ':'
  spaces
  no  <- label
  char ':'
  spaces
  act <- action
  return $ Statement (Just yes) (Just no) act

fallStatement :: Parser Statement
fallStatement = Statement Nothing Nothing `liftM` action

action :: Parser String
action = many1 (noneOf "\r\n")

label :: Parser String
label = many (noneOf ":\r\n")

eol :: Parser ()
eol = void $ try (string "\n\r") <|> try (string "\r\n") <|> string "\n" <|> string "\r"

space1 :: Parser ()
space1 = void $ space >> spaces

many1 :: Parser a -> Parser [a]
many1 p = do
  x  <- p
  xs <- many p
  return $ x:xs

