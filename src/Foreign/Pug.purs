module Foreign.Pug (CompiledPug, compileFile, compileFile', render, renderFile) where

import Data.Argonaut (Json)
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Effect (Effect)
import Foreign.Object (Object)
import Node.Path (FilePath)

foreign import data CompiledPug :: Type

compileFile :: String -> Effect CompiledPug
compileFile = runFn1 compileFileImpl

foreign import compileFileImpl :: Fn1 String (Effect CompiledPug)

render :: Object String -> CompiledPug -> String
render = runFn2 renderImpl

foreign import renderImpl :: Fn2 (Object String) CompiledPug String

compileFile' :: String -> Effect (Object String -> String)
compileFile' = runFn1 compileFileCurriedImpl

foreign import compileFileCurriedImpl :: Fn1 String (Effect (Object String -> String))

renderFile :: FilePath -> Json -> Effect String
renderFile = runFn2 renderFileImpl

foreign import renderFileImpl :: Fn2 FilePath Json (Effect String)
