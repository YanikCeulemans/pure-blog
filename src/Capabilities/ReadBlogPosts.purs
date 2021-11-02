module Capabilities.ReadBlogPosts
  ( class ReadBlogPosts
  , readBlogPostContent
  , readBlogPostsIndex
  , readBlogPost
  ) where

import Prelude

import BlogPost (BlogPost)
import Data.Map (Map)
import Slug (Slug)

class Monad m <= ReadBlogPosts m where
  readBlogPostContent :: Slug -> m String
  readBlogPostsIndex :: m (Map Slug BlogPost)
  readBlogPost :: Slug -> m BlogPost
