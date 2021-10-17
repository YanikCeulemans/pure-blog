module Main where

import Prelude

import BlogPost (BlogPost)
import BlogPost as BlogPost
import CSS as CSS
import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (Json)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.List ((:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Error, error, message)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (stack)
import Foreign.MarkdownIt as MD
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
  { renderHtmlWithTemplate :: Json -> HtmlBody
  }

readerMiddleware
  :: (HTTPure.Request -> ReaderT Env Aff HTTPure.Response)
  -> HTTPure.Request
  -> HTTPure.ResponseM
readerMiddleware router request = do
  renderHtmlWithTemplate <- liftEffect $ Pug.compileFile
    "./static/layout/main.pug"
  let
    env = { renderHtmlWithTemplate }
  catchError
    (runReaderT (router request) env)
    (\error -> runReaderT (reportAndRenderErrorPage error) env)

reportAndRenderErrorPage
  :: forall m. MonadAsk Env m => MonadAff m => Error -> m HTTPure.Response
reportAndRenderErrorPage error = do
  let
    errorMsg = fromMaybe (message error) $ stack error
  Console.error $ "An uncaught error occurred: " <> errorMsg
  renderHtmlWithTemplate <- asks _.renderHtmlWithTemplate
  errorPage <- liftEffect $ Pug.renderFile "./static/pages/error.pug" $
    Json.encodeJson
      { error:
          { status: 500
          , message: "Internal server error"
          }
      }
  HTTPure.internalServerError
    $ renderHtmlWithTemplate
    $ Json.encodeJson { contents: errorPage }

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
        "assets" : assetPath -> fileRouter "./static/assets" $ List.intercalate
          "/"
          assetPath
        _ -> renderNotFound

renderStyles :: HTTPure.ResponseM
renderStyles = do
  CSS.render Styles.main
    # CSS.renderedSheet
    # case _ of
        Just sheet ->
          HTTPure.ok' (HTTPure.header "Content-Type" "text/css") sheet
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
    Left _ -> renderNotFound
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
  mimeTypeFromExtension _ = Nothing

renderResource
  :: forall m. MonadAsk Env m => MonadAff m => String -> m HTTPure.Response
renderResource _ = HTTPure.internalServerError
  "Internal server error: Not yet implemented"

renderIndex :: forall m. MonadAsk Env m => MonadAff m => m HTTPure.Response
renderIndex =
  do
    renderHtmlWithTemplate <- asks _.renderHtmlWithTemplate
    blogPostsIndex <- liftAff $ readBlogPostsIndex "./static/blog/index.yaml"
    indexContents <- liftEffect $ Pug.renderFile "./static/pages/index.pug"
      $ Json.encodeJson { posts: blogPostsIndex }
    HTTPure.ok
      $ renderHtmlWithTemplate
      $ Json.encodeJson
          { contents: indexContents }

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
      do
        blogPostHtml <- loadBlogPostHtmlForSlug slug
        renderHtmlWithTemplate <- asks _.renderHtmlWithTemplate
        HTTPure.ok
          $ renderHtmlWithTemplate
          $ Json.encodeJson
              { contents: blogPostHtml }

loadBlogPostHtmlForSlug :: forall m. MonadAff m => Slug -> m HtmlBody
loadBlogPostHtmlForSlug slug = do
  blogPost <- liftAff $ readBlogPost slug
  postMarkdown <- liftAff $ FS.readTextFile UTF8 $ "./static/blog/posts/"
    <> Slug.toString slug
    <> ".md"
  renderedMd <- liftEffect $ MD.renderString postMarkdown
  liftEffect
    $ Pug.renderFile "./static/pages/blogpost.pug"
    $ Json.encodeJson
        { post: blogPost
        , content: renderedMd
        }

renderNotFound :: forall m. MonadAsk Env m => MonadAff m => m HTTPure.Response
renderNotFound = do
  renderHtmlWithTemplate <- asks _.renderHtmlWithTemplate
  HTTPure.response'
    HTTPureStatus.notFound
    (HTTPure.header "Content-Type" "text/html; charset=utf-8")
    $ renderHtmlWithTemplate
    $ Json.encodeJson { contents: "Not found" }

data ServerError
  = InvalidSlug String

type ErrorViewModel =
  { status :: HTTPureStatus.Status
  , statusMessage :: String
  , message :: String
  }

statusToString :: HTTPureStatus.Status -> String
statusToString 400 = "Bad request"
statusToString _ = "Unknown status code"

respondWithError
  :: forall m. MonadAsk Env m => MonadAff m => ServerError -> m HTTPure.Response
respondWithError serverError = do
  renderHtmlWithTemplate <- asks _.renderHtmlWithTemplate
  let
    error = serverErrorToErrorViewModel serverError
  errorHtml <- liftEffect $ Pug.renderFile "./static/pages/error.pug"
    $ Json.encodeJson { error }
  HTTPure.response error.status $ renderHtmlWithTemplate $ Json.encodeJson
    { contents: errorHtml }

serverErrorToErrorViewModel
  :: ServerError -> ErrorViewModel
serverErrorToErrorViewModel (InvalidSlug invalidSlug) =
  { status: 400
  , statusMessage: statusToString 400
  , message: "Invalid post slug: " <> invalidSlug
  }

main :: HTTPure.ServerM
main = do
  HTTPure.serve 8080 (readerMiddleware indexRouter) $ Console.log
    "Server running on port 8080: http://localhost:8080"
