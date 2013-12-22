module UpcParser where

import Control.Monad (void)
import Text.Parsec (many, anyChar, string, (<|>), parse, try, spaces, eof)
import Text.Parsec.String (Parser)
import Text.Parsec.Error (ParseError)
import ParserUtil

data UpcProg = UpcProg [Stmt]
  deriving (Show)

data Stmt = SBranch String (Maybe String) (Maybe String)
          | SGoto String
          | SLabel String
  deriving (Show)

-- upc_program         ::= statement "\n" (upc_program | ε)
-- statement           ::= label_statement
--                       | branch_statement
--                       | procedure
-- label_statement     ::= "__LABEL__" label
-- branch_statement    ::= if_statement
--                       | unless_statement
--                       | whichever_statement
-- if_statement        ::= "__GOTO__" label "__IF__" procedure
-- unless_statement    ::= "__GOTO__" label "__UNLESS__" procedure
-- whichever_statement ::= "__GOTO__" label "__OR__" label "__BY__" procedure
-- label               ::= [^\r\n]+

--data Step = Step (Maybe String) Stmt
--  deriving (Show)
--
--data Stmt = Stmt (Maybe String) (Maybe String) String
--  deriving (Show)

--parseUpc :: String -> Either ParseError UpcProg
--parseUpc = Right $ UpcProg []

parseUpc :: String -> Either ParseError UpcProg
parseUpc = parse upcSrc "Unstructured P--C--"

upcSrc :: Parser UpcProg
upcSrc = do
  spaces
  stats <- many pStat
  return $ UpcProg stats

trail :: Parser ()
trail = spacesLn >> (eof <|> (eol >> spaces))

pStat :: Parser Stmt
pStat = try pLabelDef
    <|> try pBranch
    <|> pFall

pLabelDef :: Parser Stmt
pLabelDef = do
  void $ string "__LABEL__"
  spaces
  l <- pLabel
  return $ SLabel l

pBranch :: Parser Stmt
pBranch = do
  void $ string "__GOTO__"
  spaces
  l <- pLabel
  try (pGotoIf l) <|> try (pGotoUnless l) <|> pGotoOrBy l

pGotoIf :: String -> Parser Stmt
pGotoIf lab = do
  void $ string "__IF__" 
  spaces
  p <- pProc
  return $ SBranch p (Just lab) Nothing

pGotoUnless :: String -> Parser Stmt
pGotoUnless lab = do
  void $ string "__UNLESS__"
  spaces
  p <- pProc
  return $ SBranch p Nothing (Just lab)

pGotoOrBy :: String -> Parser Stmt
pGotoOrBy ylab = do
  void $ string "__OR__"
  spaces
  nlab <- pLabel
  void $ string "__BY__"
  spaces
  p <- pProc
  return $ SBranch p (Just ylab) (Just nlab)

pFall :: Parser Stmt
pFall = do
  p <- pProc
  return $ SBranch p Nothing Nothing

pLabel :: Parser String
pLabel = manyTill1 anyChar (try trail)

pProc :: Parser String
pProc = manyTill1 anyChar (try trail)

--statement :: Parser Stmt
--statement = try branchStatement
--        <|> fallStatement
--
--branchStatement :: Parser Stmt
--branchStatement = try ifStatement
--              <|> try unlessStatement
--	      <|> whicheverStatement
--
--ifStatement :: Parser Stmt
--ifStatement = do
--  void $ string "__GOTO_IF__"
--  space1
--  yes <- label
--  void $ char ':'
--  spaces
--  act <- action
--  return $ Branch (Just yes) Nothing act
--
--unlessStatement :: Parser Stmt
--unlessStatement = do
--  void $ string "__GOTO_UNLESS__"
--  space1
--  no <- label
--  void $ char ':'
--  spaces
--  act <- action
--  return $ Branch Nothing (Just no) act
--
--whicheverStatement :: Parser Stmt
--whicheverStatement = do
--  void $ string "__GOTO__"
--  space1
--  yes <- label
--  void $ char ':'
--  spaces
--  no  <- label
--  void $ char ':'
--  spaces
--  act <- action
--  return $ Branch (Just yes) (Just no) act
--
--fallStatement :: Parser Stmt
--fallStatement = Branch Nothing Nothing `liftM` action
--
--action :: Parser String
--action = many1 (noneOf "\r\n")
--
--label :: Parser String
--label = many (noneOf ":\r\n")

