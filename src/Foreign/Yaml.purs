module Foreign.Yaml (parse) where

import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)

parse :: String -> Either String Json
parse = runFn3 parseImpl Left Right

foreign import parseImpl :: forall m. Fn3 (String -> m) (Json -> m) String m
