module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Foreign.MarkdownIt as MD
import Foreign.Object as Obj
import Foreign.Pug as Pug
import HTTPure as HTTPure

main :: HTTPure.ServerM
main = do
  renderTemplate <- Pug.compileFile' "./static/layout/main.pug"
  HTTPure.serve 8080 (router renderTemplate) $ Console.log "Server running on port 8080: http://localhost:8080"
  where
  router render _ = HTTPure.ok $ render $ Obj.fromFoldable []
