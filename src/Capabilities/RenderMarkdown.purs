module Capabilities.RenderMarkdown (class RenderMarkdown, renderMarkdown) where

import Prelude

import Data.Body (HtmlBody)

class Monad m <= RenderMarkdown m where
  renderMarkdown :: String -> m HtmlBody
