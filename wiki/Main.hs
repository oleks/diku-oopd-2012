module Main where

--import Text.LaTeX.Base.Syntax
import Text.Parsec
import Text.Parsec.Text
import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.FilePath as FilePath
import Data.Text hiding(map)

class Html a where
  toHtml :: a -> String

data TeXeme
  = TeXRaw Text
  | TeXCommand Text
  deriving(Show)

instance Html TeXeme where
  toHtml (TeXRaw raw) = unpack raw
  toHtml (TeXCommand _) = ""

data Paragraph
  = TeXemes [TeXeme]
  deriving(Show)

instance Html Paragraph where
  toHtml (TeXemes texemes) =
    "<p>" ++ (List.concat $ map toHtml texemes) ++ "</p>"

data LaTeX
  = Paragraphs [Paragraph]
  deriving(Show)

instance Html LaTeX where
  toHtml (Paragraphs paragraphs) =
    "<!DOCTYPE HTML><html><body>" ++
    (List.concat $ map toHtml paragraphs) ++
    "</body>"

parseLaTeX :: Text -> Either ParseError LaTeX
parseLaTeX latex = parse parseString "LaTeX input" latex

parseFileToHtml :: FilePath -> IO (Either ParseError ())
parseFileToHtml filePath =
  let
    outPath = FilePath.addExtension (FilePath.dropExtension filePath) ".html"
  in do
    input <- readFile filePath
    result <- return $ runParser parseString () filePath (pack input)
    case result of
      Right latex -> do
      {
        writeFile outPath (toHtml latex);
        return $ Right ()
      }
      Left error -> return $ Left error

parseString :: Parser LaTeX
parseString = do
  latex <- many parseParagraph
  eof
  return $ Paragraphs latex

parseParagraph :: Parser Paragraph
parseParagraph = do
  optional (char '\n')
  texemes <- many1 $
    (try parseCommand) <|>
    parseRaw
  return $ TeXemes texemes

parseCommand :: Parser TeXeme
parseCommand = do
  char '\\'
  name <- many1 alphaNum
  return $ TeXCommand (pack name)

parseRaw :: Parser TeXeme
parseRaw = do
  raw <- many1 $
    (satisfy Char.isAlphaNum) <|>
    (satisfy (\ char -> elem char [' ','.'])) <|>
    try (do { newline; notFollowedBy newline; return ' '})
  return $ TeXRaw $ (strip $ pack raw)

