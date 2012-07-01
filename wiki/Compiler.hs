module Compiler(compile) where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Map as Map
import Grammar

import Text.Show

data State = OrderedList | OrderedItem

type Stack = [State]

data Context
  = Context {
    environmentStack :: [String],
    paragraphLength :: Int,
    output :: String,
    contextParagraph :: String,
    contextItem :: Bool,
    groupStack :: [String]
  }

environmentMap :: Map.Map String (String, String)
environmentMap = Map.fromList [
  ("enumerate", ("<ol>", "</ol>")),
  ("itemize", ("<ul>", "</ol>")),
  ("code", ("<pre><code>", "</code></pre>")),
  ("definition", ("<dfn>", "</dfn>"))]

initialContext :: Context
initialContext
  = Context {
    environmentStack = [],
    paragraphLength = 0,
    output = "",
    contextParagraph = "",
    contextItem = False,
    groupStack = []
  }

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
    output context ++
    "</article></body></html>"

compileLaTeX :: LaTeX -> TeX ()
compileLaTeX (Paragraphs paragraphs) = do
  mapM compileParagraph paragraphs
  return ()

openParagraph :: TeX ()
openParagraph = do
  context <- getContext
  if inParagraph context
  then fail "Paragraph is already open."
  else return ()

closeParagraph :: TeX ()
closeParagraph = do
  context <- getContext
  if inParagraph context
  then setContext $ context {
    output = (output context) ++ "<p>" ++ (contextParagraph context) ++ "</p>",
    paragraphLength = 0,
    contextParagraph = ""
  }
  else return ()

addToParagraph :: Text.Text -> TeX ()
addToParagraph text = do
  context <- getContext
  setContext context {
    paragraphLength = (paragraphLength context) + (Text.length text),
    contextParagraph = (contextParagraph context) ++ (Text.unpack text)
  }

inParagraph :: Context -> Bool
inParagraph context = paragraphLength context > 0

openItem :: TeX ()
openItem = do
  closeParagraph
  context <- getContext
  if contextItem context
  then fail "Item is already open."
  else setContext context {
    output = (output context) ++ "<li>",
    contextItem = True
  }

closeItem :: TeX ()
closeItem = do
  closeParagraph
  context <- getContext
  if contextItem context
  then do
    setContext context {
      output = (output context) ++ "</li>",
      contextItem = False
    }
  else return ()

addToGroupStack :: String -> TeX ()
addToGroupStack name = do
  context <- getContext
  setContext context {
    groupStack = name : (groupStack context)
  }
  addToParagraph $ Text.pack ("<" ++ name ++ ">")

compileParagraph :: Paragraph -> TeX ()
compileParagraph (TeXemes texemes) = do

  openParagraph

  mapM compileTeXeme texemes

  context <- getContext
  if inParagraph context
  then closeParagraph
  else return ()

compileTeXeme :: TeXeme -> TeX ()

compileTeXeme (TeXRaw text) =
  case (Text.length text) of
    0 -> return ()
    _ -> addToParagraph text

compileTeXeme (TeXVerbatim lines) = do
  closeParagraph
  context <- getContext
  setContext context {
    output = (output context) ++ ("<code>" ++ (compileLines lines "") ++ "</code>")
  }

compileTeXeme (TeXBegin name) = do
  closeParagraph
  context <- getContext
  setContext context {
    environmentStack = name : (environmentStack context),
    output = (output context) ++ fst (environmentMap Map.! name)
  }

compileTeXeme (TeXEnd name) = do
  closeItem
  context <- getContext
  case (environmentStack context) of
    (frame : tail) ->
      if frame == name
      then do
        setContext context {
          environmentStack = tail,
          output = (output context) ++ snd (environmentMap Map.! name)
        }
      else fail "stack underflow 0"
    _ -> fail "stack underflow 1"

compileTeXeme (TeXCommand "item" _) = do
  closeItem
  openItem

compileTeXeme (TeXCommand "bf" _) = do
  addToGroupStack "b"

compileTeXeme (TeXCommand "it" _) = do
  addToGroupStack "i"

compileTeXeme (TeXGroup paragraphs) = do
  length <- getParagraphLength
  if length > 0
  then addToParagraph $ Text.pack " "
  else return ()

  mapM (\(TeXemes texemes) -> mapM compileTeXeme texemes) paragraphs

  context <- getContext

  if paragraphLength context > 0
  then do
    mapM closeGroupAux (groupStack context)
    addToParagraph $ Text.pack " "
  else return ()

compileLines :: [VerbatimLine] -> String-> String
compileLines (line : lines @ (_:_)) string =
  compileLines lines (string ++ (compileLine line) ++ "<br/>")
compileLines (line : _) string =
  string ++ (compileLine line)
compileLines _ string = string

compileLine :: VerbatimLine -> String
compileLine (VerbatimLine offset string) =
  (List.concat (replicate offset "&nbsp;")) ++ string


getParagraphLength :: TeX Int
getParagraphLength = do
  context <- getContext
  return $ paragraphLength context

closeGroupAux :: String -> TeX ()
closeGroupAux name = do
  addToParagraph $ Text.pack ("</" ++ name ++ ">")

