module Foreign.Pug (CompiledPug, compileFile, compileFile', render) where

import Prelude

import Data.Function.Uncurried (Fn2, Fn1, runFn1, runFn2)
import Effect (Effect)
import Foreign.Object (Object)

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
