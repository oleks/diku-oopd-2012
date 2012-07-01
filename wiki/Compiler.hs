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
    output =
      showString (output context) $
      showString "<p>" $
      showString (contextParagraph context) "</p>",
    paragraphLength = 0,
    contextParagraph = ""
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
  addToParagraph $ showString "<" $ showString name ">"

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
    _ -> addToParagraph $ Text.unpack text

compileTeXeme (TeXVerbatim text) = do
  closeParagraph
  context <- getContext
  setContext context {
    output =
      showString (output context) $
      showString "<pre><code>" $
      showString (Text.unpack text) "</code></pre>"
  }

compileTeXeme (TeXBegin name) = do
  closeParagraph
  context <- getContext
  setContext context {
    environmentStack = name : (environmentStack context),
    output =
      showString (output context) $
      fst (environmentMap Map.! name)
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
          output =
            showString (output context) $
            snd (environmentMap Map.! name)
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
  then addToParagraph " "
  else return ()

  mapM (\(TeXemes texemes) -> mapM compileTeXeme texemes) paragraphs

  context <- getContext

  if paragraphLength context > 0
  then do
    mapM closeGroupAux (groupStack context)
    addToParagraph " "
  else return ()

getParagraphLength :: TeX Int
getParagraphLength = do
  context <- getContext
  return $ paragraphLength context

closeGroupAux :: String -> TeX ()
closeGroupAux name = do
  addToParagraph $ showString "</" $ showString name ">"

