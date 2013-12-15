module ParserUtil where

import Control.Monad (liftM, void)
import Control.Applicative ((<$), (<$>))
import Text.Parsec (oneOf, char, string, noneOf, (<|>), parse, try, many, many1, space, spaces, eof, endBy)
import Text.Parsec.String (Parser)

eol :: Parser ()
eol = void $ try (string "\n\r") <|> try (string "\r\n") <|> string "\n" <|> string "\r"

space1 :: Parser ()
space1 = void $ many1 space

lineSpace :: Parser ()
lineSpace = void $ oneOf ['\t', '\f', '\v']

lineSpaces :: Parser ()
lineSpaces = void $ many lineSpace

manyTill1 :: Parser a -> Parser end -> Parser [a]
manyTill1 p end = do
  (ok, res) <- ((False, []) <$ end) <|> ((\xs -> (True, xs)) <$> scan)
  if ok
    then return res
    else fail "manyTill1: `end` comes before `p`"
  where
    scan = do { end; return [] } <|> do { x <- p; xs <- scan; return (x:xs) }

