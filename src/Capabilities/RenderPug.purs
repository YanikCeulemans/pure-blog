module Capabilities.RenderPug where

import Prelude

import Data.Argonaut (class EncodeJson)
import Foreign.Pug (HtmlBody)
import Node.Path (FilePath)

class Monad m <= RenderPug m where
  renderPugFile
    :: forall row
     . EncodeJson { | row }
    => FilePath
    -> { | row }
    -> m HtmlBody
