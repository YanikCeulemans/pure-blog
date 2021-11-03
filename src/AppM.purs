module AppM (AppM, Env, runAppM) where

import Prelude

import BlogPost (BlogPost)
import BlogPost as BlogPost
import Capabilities.LogMessages (class LogMessages)
import Capabilities.Now (class Now)
import Capabilities.ReadBlogPosts (class ReadBlogPosts, readBlogPostsIndex)
import Capabilities.RenderMarkdown (class RenderMarkdown)
import Capabilities.RenderPug (class RenderPug)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.Argonaut (Json)
import Data.Argonaut as Json
import Data.Array (fold)
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Log as Log
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
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
  readBlogPostContent slug =
    liftAff $ FS.readTextFile UTF8
      $ fold [ "./static/blog/posts/", Slug.toString slug, ".md" ]

  readBlogPostsIndex = liftAff do
    indexJsonEither <- Yaml.parse <$> FS.readTextFile UTF8
      "./static/blog/index.yaml"
    either (throwError <<< error) pure do
      indexJson <- indexJsonEither
      rawBlogPosts <- lmap Json.printJsonDecodeError $ Json.decodeJson indexJson
      traverse BlogPost.fromRawBlogPost rawBlogPosts
        # lmap BlogPost.printBlogPostDecodeError
        # map toBlogPostIndex
    where
    toBlogPostIndex :: Array BlogPost -> Map Slug BlogPost
    toBlogPostIndex = map toSlugIndexedPost >>> Map.fromFoldable
    toSlugIndexedPost post = Tuple (BlogPost.slug post) post

  readBlogPost slug = do
    postsIndex <- readBlogPostsIndex
    case Map.lookup slug postsIndex of
      Just post -> pure post
      Nothing ->
        -- TODO: We don't want to throw errors in Aff, this should probably run in EitherT
        throwError $ error $ fold
          [ "No post could be found for slug: ", show slug ]

instance RenderMarkdown AppM where
  renderMarkdown = liftEffect <<< map HtmlBody <<< MD.renderString

instance RenderPug AppM where
  renderPugFile filePath = liftEffect <<< Pug.renderFile filePath

runAppM :: forall a. AppM a -> Env -> Aff a
runAppM (AppM reader) = runReaderT reader
