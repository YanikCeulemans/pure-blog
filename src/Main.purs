module Main where

import Prelude

import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Control.Monad.Reader.Class (asks)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.MarkdownIt as MD
import Foreign.Object as Obj
import Foreign.Pug as Pug
import HTTPure as HTTPure

type Env =
  { renderHtmlWithTemplate :: Obj.Object String -> String
  }

readerMiddleware
  :: (HTTPure.Request -> ReaderT Env Aff HTTPure.Response)
  -> HTTPure.Request
  -> HTTPure.ResponseM
readerMiddleware router request = do
  renderHtmlWithTemplate <- liftEffect $ Pug.compileFile' "./static/layout/main.pug"
  let
    env = { renderHtmlWithTemplate }
  runReaderT (router request) env

indexRouter :: forall m. MonadAsk Env m => MonadAff m => HTTPure.Request -> m HTTPure.Response
indexRouter _ = do
  render <- asks _.renderHtmlWithTemplate
  HTTPure.ok $ render $ Obj.fromFoldable []

main :: HTTPure.ServerM
main = do
  HTTPure.serve 8080 (readerMiddleware indexRouter) $ Console.log "Server running on port 8080: http://localhost:8080"
