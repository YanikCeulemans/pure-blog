module Main where

import Prelude

import BlogPost (BlogPost)
import BlogPost as BlogPost
import CSS as CSS
import Capabilities.LogMessages (class LogMessages)
import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (Json)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Function (applyFlipped)
import Data.List ((:))
import Data.List as List
import Data.Log as Log
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.Utils as StringUtils
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Error, error, message)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (stack)
import Foreign.MarkdownIt as MD
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Pug (HtmlBody)
import Foreign.Pug as Pug
import Foreign.Yaml as Yaml
import HTTPure as HTTPure
import HTTPure.Status as HTTPureStatus
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path
import Slug (Slug)
import Slug as Slug
import Styles.Main as Styles

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

instance LogMessages AppM where
  logMessage log = do
    Console.log $ Log.message log

runAppM :: forall a. AppM a -> Env -> Aff a
runAppM (AppM reader) = runReaderT reader

renderLayout :: forall m. MonadAsk Env m => HtmlBody -> m HtmlBody
renderLayout =
  Json.encodeJson
    >>> { contents: _ }
    >>> Object.fromHomogeneous
    >>> render
  where
  render obj = asks _.renderHtmlWithTemplate <#> applyFlipped obj

readerMiddleware
  :: (HTTPure.Request -> AppM HTTPure.Response)
  -> HTTPure.Request
  -> HTTPure.ResponseM
readerMiddleware router request = do
  renderHtmlWithTemplate <- liftEffect $ Pug.compileFile
    "./static/layout/main.pug"
  let
    env = { renderHtmlWithTemplate }
  catchError
    (runAppM (router request) env)
    (\error -> runReaderT (reportAndRenderErrorPage error) env)

reportAndRenderErrorPage
  :: forall m. MonadAsk Env m => MonadAff m => Error -> m HTTPure.Response
reportAndRenderErrorPage error = do
  let
    errorMsg = fromMaybe (message error) $ stack error
  Console.error $ "An uncaught error occurred: " <> errorMsg
  errorPage <- liftEffect
    $ Pug.renderFile "./static/pages/error.pug"
        { error:
            { status: 500
            , message: "Internal server error"
            }
        }
  html <- renderLayout errorPage
  HTTPure.internalServerError html

indexRouter
  :: forall m
   . MonadAsk Env m
  => MonadAff m
  => HTTPure.Request
  -> m HTTPure.Response
indexRouter request = do
  case request.path of
    [] -> renderIndex
    [ "css", "styles.css" ] -> liftAff renderStyles
    [ "blog", unvalidatedSlug ] -> renderBlogPost unvalidatedSlug
    _ ->
      case List.fromFoldable request.path of
        "assets" : assetPath ->
          fileRouter "./static/assets" $ intercalate "/" assetPath
        _ -> respondWithError $ NotFound $ intercalate "/" request.path

renderStyles :: HTTPure.ResponseM
renderStyles = do
  CSS.render Styles.main
    # CSS.renderedSheet
    # case _ of
        Just sheet ->
          HTTPure.ok'
            (HTTPure.header "Content-Type" "text/css; charset=utf-8")
            sheet
        Nothing ->
          HTTPure.internalServerError "Internal server error"

fileRouter
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => String
  -> String
  -> m HTTPure.Response
fileRouter basePath filePath = do
  let
    fullPath = Path.concat [ basePath, filePath ]
  fileBufferE <- liftAff $ try $ FS.readFile fullPath
  case fileBufferE of
    Left _ -> respondWithError $ NotFound filePath
    Right fileBuffer -> do
      let
        headers =
          [ mimeTypeFromPath filePath <#> Tuple "Content-Type" ]
            # Array.mapMaybe identity
            # HTTPure.headers
      HTTPure.ok' headers fileBuffer

mimeTypeFromPath :: String -> Maybe String
mimeTypeFromPath =
  String.split (String.Pattern ".")
    >>> Array.reverse
    >>> List.fromFoldable
    >>> case _ of
      ext : _ -> mimeTypeFromExtension ext
      _ -> Nothing
  where
  mimeTypeFromExtension "png" = Just "image/png"
  mimeTypeFromExtension "css" = Just "text/css; charset=utf-8"
  mimeTypeFromExtension _ = Nothing

renderResource
  :: forall m. MonadAsk Env m => MonadAff m => String -> m HTTPure.Response
renderResource _ = HTTPure.internalServerError
  "Internal server error: Not yet implemented"

renderIndex :: forall m. MonadAsk Env m => MonadAff m => m HTTPure.Response
renderIndex =
  do
    blogPostsIndex <- liftAff $ readBlogPostsIndex "./static/blog/index.yaml"
    indexContents <- liftEffect
      $ Pug.renderFile "./static/pages/index.pug" { posts: blogPostsIndex }
    html <- renderLayout indexContents
    HTTPure.ok html

readBlogPostsIndex :: FilePath -> Aff (Array BlogPost)
readBlogPostsIndex indexFilePath = do
  indexJsonE <- Yaml.parse <$> FS.readTextFile UTF8 indexFilePath
  either (throwError <<< error) pure $ do
    indexJson <- indexJsonE
    rawBlogPosts <- lmap Json.printJsonDecodeError $ Json.decodeJson indexJson
    lmap BlogPost.printBlogPostDecodeError
      $ traverse BlogPost.fromRawBlogPost rawBlogPosts

readBlogPost :: Slug -> Aff BlogPost
readBlogPost slug = do
  blogPostsIndex <- readBlogPostsIndex "./static/blog/index.yaml"
  case Array.find (BlogPost.slug >>> (==) slug) blogPostsIndex of
    Nothing -> throwError $ error
      ( "No post could be found for slug: '"
          <> Slug.toString slug
          <> "'"
      )
    Just post -> pure post

renderBlogPost
  :: forall m. MonadAsk Env m => MonadAff m => String -> m HTTPure.Response
renderBlogPost unvalidatedSlug =
  case Slug.fromString unvalidatedSlug of
    Nothing -> respondWithError $ InvalidSlug unvalidatedSlug
    Just slug ->
      HTTPure.ok =<< renderLayout =<< loadBlogPostHtmlForSlug slug

loadBlogPostHtmlForSlug :: forall m. MonadAff m => Slug -> m HtmlBody
loadBlogPostHtmlForSlug slug = do
  blogPost <- liftAff $ readBlogPost slug
  postMarkdown <- liftAff $ FS.readTextFile UTF8 $ "./static/blog/posts/"
    <> Slug.toString slug
    <> ".md"
  renderedMd <- liftEffect $ MD.renderString postMarkdown
  liftEffect
    $ Pug.renderFile "./static/pages/blogpost.pug"
        { post: blogPost
        , readTimeInMinutes: calculateAvgReadTime postMarkdown
        , content: renderedMd
        }

calculateAvgReadTime :: String -> Int
calculateAvgReadTime =
  StringUtils.words
    >>> Array.length
    >>> divByAvgWordsPerMinute
  where
  divByAvgWordsPerMinute = flip div 250

data ServerError
  = InvalidSlug String
  | NotFound String

type ErrorViewModel =
  { status :: HTTPureStatus.Status
  , statusMessage :: String
  , message :: String
  }

statusToString :: HTTPureStatus.Status -> String
statusToString 400 = "Bad request"
statusToString 404 = "Not found"
statusToString _ = "Unknown status code"

respondWithError
  :: forall m. MonadAsk Env m => MonadAff m => ServerError -> m HTTPure.Response
respondWithError serverError = do
  let
    error = serverErrorToErrorViewModel serverError
  errorHtml <- liftEffect $ Pug.renderFile "./static/pages/error.pug" { error }
  html <- renderLayout errorHtml
  HTTPure.response error.status html

serverErrorToErrorViewModel
  :: ServerError -> ErrorViewModel
serverErrorToErrorViewModel (InvalidSlug invalidSlug) =
  { status: 400
  , statusMessage: statusToString 400
  , message: "Invalid post slug: " <> invalidSlug
  }
serverErrorToErrorViewModel (NotFound notFoundPath) =
  { status: 404
  , statusMessage: statusToString 404
  , message: "Not found: " <> notFoundPath
  }

main :: HTTPure.ServerM
main = do
  HTTPure.serve 8080 (readerMiddleware indexRouter) $ Console.log
    "Server running on port 8080: http://localhost:8080"
