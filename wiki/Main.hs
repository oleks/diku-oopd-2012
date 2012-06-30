module Main where

import Text.Parsec
import Text.Parsec.Text

import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.FilePath as FilePath
import Data.Text hiding(map)

import Grammar
import Parser
import Compiler

parseLaTeX :: Text -> Either ParseError LaTeX
parseLaTeX latex = parse parseString "LaTeX input" latex

parseFile :: FilePath -> IO (Either ParseError LaTeX)
parseFile filePath = do
  input <- readFile filePath
  return $ runParser parseString () filePath (pack input)

compileFileToHtml :: FilePath -> IO (Either ParseError ())
compileFileToHtml filePath =
  let
    outPath = FilePath.addExtension (FilePath.dropExtension filePath) ".html"
  in do
    result <- parseFile filePath
    case result of
      Right latex -> do
      {
        writeFile outPath (compile latex);
        return $ Right ()
      }
      Left error -> return $ Left error

compileFile :: FilePath -> IO String
compileFile filePath = do
  result <- parseFile filePath
  case result of
    Right latex -> return $ compile latex
    Left errorMessage -> error $ show $ errorPos errorMessage

