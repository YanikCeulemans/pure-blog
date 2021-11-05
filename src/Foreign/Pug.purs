module Foreign.Pug (compileFile, renderFile) where

import Prelude

import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.Functor.Contravariant (cmap)
import Data.Newtype (un)
import Data.Op (Op(..))
import Data.Body (HtmlBody)
import Effect (Effect)
import Foreign.Object (Object)
import Node.Path (FilePath)

compileFile :: FilePath -> Effect (Object Json -> HtmlBody)
compileFile =
  runFn1 compileFileImpl
    >>> map (Op >>> cmap encodeJson >>> un Op)

foreign import compileFileImpl :: Fn1 FilePath (Effect (Json -> HtmlBody))

renderFile
  :: forall row
   . EncodeJson { | row }
  => FilePath
  -> { | row }
  -> Effect HtmlBody
renderFile filePath = runFn2 renderFileImpl filePath <<< encodeJson

foreign import renderFileImpl :: Fn2 FilePath Json (Effect HtmlBody)
