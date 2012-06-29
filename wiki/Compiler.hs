module Compiler(compile) where

--import Data.Foldable
import qualified Data.List as List
import qualified Data.Text as Text
import Grammar

data State = OrderedList | OrderedItem

type Stack = [State]

data Context
  = Context {
    environmentStack :: [String],
    paragraphLength :: Int,
    output :: String,
    contextParagraph :: String
  }

initialContext :: Context
initialContext
  = Context {
    environmentStack = [],
    paragraphLength = 0,
    output = "",
    contextParagraph = ""
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

{--
getContextStack :: TeX Stack
getContextStack = getFromContext contextStack

setContextStack :: Stack -> TeX ()
setContextStack stack =
  TeX { runTeX = \context -> ((), context { contextStack = stack }) }

pushState :: State -> TeX ()
pushState state = do
  stack <- getContextStack
  setContextStack (state : stack)

popState :: TeX State
popState = do
  stack <- getContextStack
  case stack of
    (state : tail) -> do
      setContextStack tail
      retrun state
    _ -> fail "stack underflow"
--}

compile :: LaTeX -> String
compile latex =
  let
    (_, context) = (runTeX (compileLaTeX latex)) initialContext
  in
    output context

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

compileTeXeme (TeXBegin name) = do
  closeParagraph
  context <- getContext
  setContext context {
    environmentStack = name : (environmentStack context),
    output = (output context) ++ "<ol>"
  }

compileTeXeme (TeXEnd name) = do
  closeParagraph
  context <- getContext
  stack <- getFromContext environmentStack
  case stack of
    (frame : tail) ->
      if frame == name
      then do
        setContext context {
          environmentStack = tail,
          output = (output context) ++ "</ol>"
        }
      else fail "stack underflow 0"
    _ -> fail "stack underflow 1"
