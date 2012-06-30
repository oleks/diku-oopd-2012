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
  optional (char '\n')
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
  group <- parseGroup <|> return []
  return $ TeXCommand name group

parseBegin :: Parser TeXeme
parseBegin = parseSpecialCommand "begin" TeXBegin

parseEnd :: Parser TeXeme
parseEnd = parseSpecialCommand "end" TeXEnd

parseSpecialCommand :: String -> (String -> TeXeme) -> Parser TeXeme
parseSpecialCommand command constructor = do
  try $ string command
  char '{'
  name <- many1 alphaNum
  char '}'
  return $ constructor name

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
      elem char [' ','.',',','-',':',';','\'','(',')'])) <|>
    try (do { newline; notFollowedBy newline; return ' '})
  return $ TeXRaw $ strip $ pack raw
