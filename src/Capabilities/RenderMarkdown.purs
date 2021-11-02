module Capabilities.RenderMarkdown (class RenderMarkdown, renderMarkdown) where

import Prelude

import Foreign.Pug (HtmlBody)

class Monad m <= RenderMarkdown m where
  renderMarkdown :: String -> m HtmlBody
