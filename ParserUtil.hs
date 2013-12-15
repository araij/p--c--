module ParserUtil where

import Control.Monad (void)
import Control.Applicative ((<$), (<$>))
import Text.Parsec (oneOf, string, (<|>), try, many, many1, space)
import Text.Parsec.String (Parser)

eol :: Parser ()
eol = void $ try (string "\n\r") <|> try (string "\r\n") <|> string "\n" <|> string "\r"

space1 :: Parser ()
space1 = void $ many1 space

spaceLn :: Parser ()
spaceLn = void $ oneOf ['\t', '\f', '\v']

spacesLn :: Parser ()
spacesLn = void $ many spaceLn

manyTill1 :: Parser a -> Parser end -> Parser [a]
manyTill1 p end = do
  (ok, res) <- ((False, []) <$ end) <|> ((\xs -> (True, xs)) <$> scan)
  if ok
    then return res
    else fail "manyTill1: `end` comes before `p`"
  where
    scan = do { void end; return [] } <|> do { x <- p; xs <- scan; return (x:xs) }

