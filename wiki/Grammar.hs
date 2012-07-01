module Grammar where

import Data.Text hiding(map)
import qualified Data.List as List

data VerbatimLine
  = VerbatimLine Int String
  deriving(Show)

data TeXeme
  = TeXRaw Text
  | TeXVerbatim [VerbatimLine]
  | TeXBegin String
  | TeXEnd String
  | TeXCommand String [Paragraph]
  | TeXGroup [Paragraph]
  deriving(Show)

data Paragraph
  = TeXemes [TeXeme]
  deriving(Show)

data LaTeX
  = Paragraphs [Paragraph]
  deriving(Show)
