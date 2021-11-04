module AppM (AppM, Env, runAppM) where

import Prelude

import BlogPost (BlogPostMetaData, blogPost)
import BlogPost as BlogPost
import Capabilities.LogMessages (class LogMessages)
import Capabilities.Now (class Now)
import Capabilities.ReadBlogPosts (class ReadBlogPosts, readBlogIndex)
import Capabilities.RenderMarkdown (class RenderMarkdown)
import Capabilities.RenderPug (class RenderPug)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (Json)
import Data.Argonaut as Json
import Data.Array (fold)
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Log as Log
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Error, error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import Foreign.MarkdownIt as MD
import Foreign.Object (Object)
import Foreign.Pug (HtmlBody(..))
import Foreign.Pug as Pug
import Foreign.Yaml as Yaml
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Slug (Slug)
import Slug as Slug

type Env =
  { renderHtmlWithTemplate :: Object Json -> HtmlBody
  }

newtype AppM a = AppM (ReaderT Env Aff a)

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Bind AppM
derive newtype instance Applicative AppM
derive newtype instance Monad AppM
derive newtype instance MonadAsk Env AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadThrow Error AppM
derive newtype instance MonadError Error AppM

instance Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance LogMessages AppM where
  logMessage log = do
    Console.log $ Log.message log

instance ReadBlogPosts AppM where
  readBlogIndex = liftAff do
    indexJsonEither <- Yaml.parse <$> FS.readTextFile UTF8
      "./static/blog/index.yaml"
    either (throwError <<< error) pure do
      indexJson <- indexJsonEither
      rawBlogPosts <- lmap Json.printJsonDecodeError $ Json.decodeJson indexJson
      traverse BlogPost.fromRawBlogPostMetaData rawBlogPosts
        # lmap BlogPost.printBlogPostDecodeError
        # map toBlogIndex
    where
    toBlogIndex :: Array BlogPostMetaData -> Map Slug BlogPostMetaData
    toBlogIndex = map toSlugIndexedPost >>> Map.fromFoldable
    toSlugIndexedPost post = Tuple (BlogPost.slug post) post

  readBlogPost slug = runMaybeT do
    metaData <- MaybeT $ Map.lookup slug <$> readBlogIndex
    content <- lift $ liftAff $ FS.readTextFile UTF8
      $ fold [ "./static/blog/posts/", Slug.toString slug, ".md" ]
    pure $ blogPost metaData content

instance RenderMarkdown AppM where
  renderMarkdown = liftEffect <<< map HtmlBody <<< MD.renderString

instance RenderPug AppM where
  renderPugFile filePath = liftEffect <<< Pug.renderFile filePath

runAppM :: forall a. AppM a -> Env -> Aff a
runAppM (AppM reader) = runReaderT reader
