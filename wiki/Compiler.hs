module Compiler(compile) where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Foldable as Foldable
import Text.Show

import Grammar

data State = OrderedList | OrderedItem

type Stack = [State]

data Context
  = Context {
    environmentStack :: [String],
    paragraphLength :: Int,
    output :: [String],
    contextParagraph :: String,
    contextTeXemes :: [TeXeme],
    contextItem :: Bool,
    groupStack :: [String],
    headerDepth :: Int,
    headerStack :: [Int]
  }

initialContext :: Context
initialContext
  = Context {
    environmentStack = [],
    paragraphLength = 0,
    output = [],
    contextParagraph = "",
    contextTeXemes = [],
    contextItem = False,
    groupStack = [],
    headerDepth = 0,
    headerStack = []
  }

environmentMap :: Map.Map String (String, String)
environmentMap = Map.fromList [
  ("enumerate", ("<ol>", "</ol>")),
  ("itemize", ("<ul>", "</ul>")),
  ("definition", ("<dfn>", "</dfn>"))]

data TeX t = TeX {
  runTeX :: Context -> (t, Context)
}

instance Monad TeX where
  return value = TeX { runTeX = \context -> (value, context) }
  tex >>= f = TeX { runTeX = \context ->
    let
      (value, newContext) = (runTeX tex) context
      newTeX = f value
    in
      (runTeX newTeX) newContext
    }

getContext :: TeX Context
getContext =
  TeX { runTeX = \context -> (context, context) }

setContext :: Context -> TeX ()
setContext context = do
  TeX { runTeX = \_ -> ((), context) }

updateContext :: (Context -> Context) -> TeX ()
updateContext f = do
  TeX { runTeX = \context -> ((), f context) }

getFromContext :: (Context -> t) -> TeX t
getFromContext f = do
  context <- getContext
  return $ f context

compile :: LaTeX -> String
compile latex =
  let
    (_, context) = (runTeX (compileLaTeX latex)) initialContext
  in
    "<!DOCTYPE html><html>" ++
    "<head><link rel='stylesheet' href='style.css'>" ++
    "<body><article>" ++
    (List.foldl' (\text i -> showString i text) "" (output context)) ++
    "</article></body></html>"

compileLaTeX :: LaTeX -> TeX ()
compileLaTeX (Paragraphs paragraphs) = do
  mapM compileParagraph paragraphs
  return ()

openParagraph :: [TeXeme] -> TeX ()
openParagraph texemes = do
  context <- getContext
  if inParagraph context
  then fail "Paragraph is already open."
  else setContext context {
      contextTeXemes = texemes
    }

closeParagraph :: TeX ()
closeParagraph = do
  context <- getContext
  if inParagraph context
  then do
    addManyToOutput ["<p>", (contextParagraph context), "</p>"]
    context <- getContext
    setContext context {
      paragraphLength = 0,
      contextParagraph = "",
      contextTeXemes = []
    }
  else return ()

simpleCloseParagraph :: TeX ()
simpleCloseParagraph = do
  context <- getContext
  if inParagraph context
  then do
    addToOutput (contextParagraph context)
    context <- getContext
    setContext $ context {
      paragraphLength = 0,
      contextParagraph = "",
      contextTeXemes = []
  }
  else return ()

addToParagraph :: String -> TeX ()
addToParagraph string = do
  context <- getContext
  setContext context {
    paragraphLength = (paragraphLength context) + (length string),
    contextParagraph =
      showString (contextParagraph context) string
  }

inParagraph :: Context -> Bool
inParagraph context = paragraphLength context > 0

openItem :: TeX ()
openItem = do
  closeParagraph
  context <- getContext
  if contextItem context
  then fail "Item is already open."
  else do
    addToOutput "<li>"
    context <- getContext
    setContext context {
      contextItem = True
    }

closeItem :: TeX ()
closeItem = do
  closeParagraph
  context <- getContext
  if contextItem context
  then do
    addToOutput "</li>"
    context <- getContext
    setContext context {
      contextItem = False
    }
  else return ()

addToGroupStack :: String -> TeX ()
addToGroupStack name = do
  context <- getContext
  setContext context {
    groupStack = name : (groupStack context)
  }
  addToParagraph $ showString "<" $ showString name ">"

compileParagraph :: Paragraph -> TeX ()
compileParagraph (TeXemes texemes) = do

  openParagraph texemes

  compileTeXemes texemes

  context <- getContext
  if inParagraph context
  then closeParagraph
  else return ()

compileTeXemes :: [TeXeme] -> TeX ()
compileTeXemes [] = return ()
compileTeXemes ((TeXRaw text):tail) = do
  case (Text.length text) of
    0 -> return ()
    _ -> do
      addParagraphSpace
      addToParagraph $ Text.unpack text
  compileTeXemes tail
compileTeXemes ((TeXVerbatim text):tail) = do
  closeParagraph
  addManyToOutput ["<pre><code>", (Text.unpack text), "</code></pre>"]
  compileTeXemes tail
compileTeXemes ((TeXBegin name):tail) = do
  closeParagraph
  addToOutput $ fst $ environmentMap Map.! name
  context <- getContext
  setContext context {
    environmentStack = name : (environmentStack context)
  }
  compileTeXemes tail
compileTeXemes ((TeXEnd name):tail) = do
  closeItem
  context <- getContext
  case (environmentStack context) of
    (frame : tail) ->
      if frame == name
      then do
        addToOutput $ snd (environmentMap Map.! name)
        context <- getContext
        setContext context {
          environmentStack = tail
        }
      else fail "stack underflow 0"
    _ -> fail "stack underflow 1"
  context <- getContext
  compileTeXemes tail
compileTeXemes (
  (TeXCommand "chapter") :
  (TeXGroup title) :
  tail) = do
    closeParagraph
    advanceHeader 1
    numeral <- getHeaderNumeral
    addManyToOutput ["<h1>", numeral]
    compileTeXGroup title
    simpleCloseParagraph
    addToOutput "</h1>"
    compileTeXemes tail
compileTeXemes (
  (TeXCommand "item") :
  tail) = do
    closeItem
    openItem
    compileTeXemes tail
compileTeXemes (
  (TeXCommand "bf") :
  tail) = do
    addToGroupStack "b"
    compileTeXemes tail
compileTeXemes (
  (TeXCommand "it") :
  tail) = do
    addToGroupStack "i"
    compileTeXemes tail
compileTeXemes (
  (TeXCommand "emph") :
  (TeXGroup paragraphs) :
  tail) = do
    addParagraphSpace
    addToParagraph "<em>"
    compileTeXGroup paragraphs
    addToParagraph "</em>"
    compileTeXemes tail
compileTeXemes (
  (TeXCommand "var") :
  (TeXGroup paragraphs) :
  tail) = do
    addParagraphSpace
    addToParagraph "<var>"
    compileTeXGroup paragraphs
    addToParagraph "</var>"
    compileTeXemes tail
compileTeXemes (
  (TeXCommand "wikipedia") :
  (TeXGroup page) :
  (TeXGroup text) :
  tail) = do
    addParagraphSpace
    addToParagraph "<a target='_blank' href='http://en.wikipedia.org/wiki/"
    compileTeXGroup page
    addToParagraph "'>"
    compileTeXGroup text
    addToParagraph "</a>"
    compileTeXemes tail
compileTeXemes (
  (TeXCommand "explanation") :
  (TeXGroup text) :
  (TeXGroup explanation) :
  tail) = do
    addParagraphSpace
    addToParagraph "<span class='tooltip' title='"
    compileTeXGroup explanation
    addToParagraph "'>"
    compileTeXGroup text
    addToParagraph "</span>"
    compileTeXemes tail
compileTeXemes (
  (TeXGroup paragraphs) :
  tail) = do
    addParagraphSpace
    compileTeXGroup paragraphs
    compileTeXemes tail

addParagraphSpace :: TeX ()
addParagraphSpace = do
  length <- getParagraphLength
  if length > 0
  then addToParagraph " "
  else return ()

compileTeXGroup :: [Paragraph] -> TeX ()
compileTeXGroup paragraphs = do
  mapM (\(TeXemes texemes) -> compileTeXemesAux texemes) paragraphs

  context <- getContext
  mapM closeGroupAux (groupStack context)
  context <- getContext
  setContext context {
    groupStack = []
  }

compileTeXemesAux :: [TeXeme] -> TeX ()
compileTeXemesAux texemes = do
  context <- getContext
  oldTeXemes <- return $ contextTeXemes context
  setContext context {
    contextTeXemes = texemes
  }

  compileTeXemes texemes

  context <- getContext
  setContext context {
    contextTeXemes = oldTeXemes
  }

addToOutput :: String -> TeX ()
addToOutput string = do
  context <- getContext
  setContext context {
    output = string : (output context)
  }

addManyToOutput :: [String] -> TeX ()
addManyToOutput strings = do
  context <- getContext
  setContext context {
    output = List.foldl' (\acc i -> i : acc) (output context) strings
  }

getParagraphLength :: TeX Int
getParagraphLength = do
  context <- getContext
  return $ paragraphLength context

closeGroupAux :: String -> TeX ()
closeGroupAux name = do
  addToParagraph $ showString "</" $ showString name ">"

advanceHeader :: Int -> TeX ()
advanceHeader depth = do
  context <- getContext
  if headerDepth context < 1
  then setContext context {
    headerDepth = (headerDepth context) + 1,
    headerStack = 0 : (headerStack context)
  }
  else
    case (headerStack context) of
      [] -> fail "header stack underflow"
      (currentNumber:tail) -> setContext context {
        headerStack = (currentNumber + 1) : tail
      }

getHeaderNumeral :: TeX String
getHeaderNumeral = do
  context <- getContext
  return $ let
      numbers = headerStack context
    in
      showString "<a class='margin' name='S." $
      prependNumbers numbers $
      showString "'>&sect; " $
      prependNumbers numbers "&nbsp;</a>"

prependNumbers :: [Int] -> String -> String
prependNumbers numbers text =
  List.foldl' (\text i -> showString (show i) ('.' : text)) text numbers
