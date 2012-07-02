module Parser where

import Text.Parsec
import Text.Parsec.Text
import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.FilePath as FilePath
import Data.Text hiding(map)

import Grammar

parseString :: Parser LaTeX
parseString = do
  latex <- many parseParagraph
  eof
  return $ Paragraphs latex

parseParagraph :: Parser Paragraph
parseParagraph = do
  many (char '\n')
  texemes <- many1 parseTeXeme
  return $ TeXemes texemes

parseTeXeme :: Parser TeXeme
parseTeXeme =
  (try parseCommand) <|>
  do { texemes <- parseGroup; return (TeXGroup texemes) } <|>
  parseRaw

parseCommand :: Parser TeXeme
parseCommand = do
  char '\\'
  parseBegin <|> parseEnd <|> parseGenericCommand

parseGenericCommand :: Parser TeXeme
parseGenericCommand = do
  name <- many1 alphaNum
  return $ TeXCommand name

parseBegin :: Parser TeXeme
parseBegin = do
  try $ string "begin{"
  name <- many1 alphaNum
  char '}'
  if name == "code"
  then do
    optional (char '\n')
    code <- manyTill anyChar (try parseEndCode)
    return $ TeXVerbatim $ pack code
  else return $ TeXBegin name

parseEndCode :: Parser String
parseEndCode = do
  optional (char '\n')
  string "\\end{code}"

parseEnd :: Parser TeXeme
parseEnd = do
  try $ string "end{"
  name <- many1 alphaNum
  char '}'
  return $ TeXEnd name

parseGroup :: Parser [Paragraph]
parseGroup = do
  char '{'
  paragraphs <- many parseParagraph
  char '}'
  return paragraphs

parseRaw :: Parser TeXeme
parseRaw = do
  raw <- many1 $
    (satisfy Char.isAlphaNum) <|>
    (satisfy (\ char ->
      elem char [
        ' ',
        '.',
        '!',
        '?',
        ',',
        '-',
        ':',
        ';',
        '\'',
        '(',
        ')'
      ])) <|>
    try (do { newline; notFollowedBy newline; return ' '})
  return $ TeXRaw $ strip $ pack raw

