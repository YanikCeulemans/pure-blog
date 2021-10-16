module Foreign.Pug (HtmlBody, compileFile, renderFile) where

import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.Semigroup (append)
import Effect (Effect)
import HTTPure (header)
import HTTPure.Body (class Body, defaultHeaders, write)
import Node.Path (FilePath)
import Prelude ((<$>))

newtype HtmlBody = HtmlBody String

instance Body HtmlBody where
  defaultHeaders (HtmlBody htmlString) =
    append htmlHeader <$> defaultHeaders htmlString
    where
    htmlHeader = header "Content-Type" "text/html; charset=utf-8"
  write (HtmlBody htmlString) r = write htmlString r

instance EncodeJson HtmlBody where
  encodeJson (HtmlBody htmlString) = encodeJson htmlString

compileFile :: FilePath -> Effect (Json -> HtmlBody)
compileFile = runFn1 compileFileImpl

foreign import compileFileImpl :: Fn1 FilePath (Effect (Json -> HtmlBody))

renderFile :: FilePath -> Json -> Effect HtmlBody
renderFile = runFn2 renderFileImpl

foreign import renderFileImpl :: Fn2 FilePath Json (Effect HtmlBody)
