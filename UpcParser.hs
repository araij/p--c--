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
--                       | expression
-- label_statement     ::= "__LABEL__" label
-- branch_statement    ::= if_statement
--                       | unless_statement
--                       | whichever_statement
-- if_statement        ::= "__GOTO__" label "__IF__" expression
-- unless_statement    ::= "__GOTO__" label "__UNLESS__" expression
-- whichever_statement ::= "__GOTO__" label "__OR__" label "__BY__" expression
-- label               ::= [^\r\n]+

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

