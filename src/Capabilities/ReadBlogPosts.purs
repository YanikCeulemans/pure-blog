module Capabilities.ReadBlogPosts
  ( class ReadBlogPosts
  , readBlogIndex
  , readBlogPost
  ) where

import Prelude

import BlogPost (BlogPost, BlogPostMetaData)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Slug (Slug)

class Monad m <= ReadBlogPosts m where
  readBlogIndex :: m (Map Slug BlogPostMetaData)
  readBlogPost :: Slug -> m (Maybe BlogPost)
