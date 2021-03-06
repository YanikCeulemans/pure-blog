module Main where

import Prelude

import AppM (AppM, Env, runAppM)
import BlogPost (BlogPost)
import BlogPost as BlogPost
import CSS as CSS
import Capabilities.LogMessages (class LogMessages, logError, logInfo)
import Capabilities.Now (class Now, now)
import Capabilities.ReadAssets (class ReadAssets, readAsset)
import Capabilities.ReadBlogPosts (class ReadBlogPosts, readBlogPost, readBlogIndex)
import Capabilities.RenderMarkdown (class RenderMarkdown, renderMarkdown)
import Capabilities.RenderPug (class RenderPug, renderPugFile)
import Control.Monad.Error.Class (class MonadError, catchError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.Reader.Class (asks)
import Data.Argonaut as Json
import Data.Array (fold)
import Data.Array as Array
import Data.Body (AssetBody, CssBody, HtmlBody)
import Data.Body as Body
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), note)
import Data.Environment (Environment(..), parseEnvironment)
import Data.Foldable (intercalate)
import Data.Function (applyFlipped, on)
import Data.Int as Int
import Data.List ((:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Newtype (un)
import Data.String as String
import Data.String.Utils as StringUtils
import Data.Time.Duration as DurationTime
import Effect.Aff (Error, message)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (stack)
import Foreign.Object as Object
import Foreign.Pug as Pug
import HTTPure as HTTPure
import HTTPure.Status as HTTPureStatus
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Partial.Unsafe (unsafeCrashWith)
import Slug (Slug)
import Slug as Slug
import Styles.Main as Styles

renderLayout :: forall m. MonadAsk Env m => HtmlBody -> m HtmlBody
renderLayout =
  Json.encodeJson
    >>> { contents: _ }
    >>> Object.fromHomogeneous
    >>> render
  where
  render obj = asks _.renderHtmlWithTemplate <#> applyFlipped obj

reportAndRenderErrorPage
  :: forall m
   . MonadAsk Env m
  => MonadAff m
  => LogMessages m
  => Now m
  => Error
  -> m HTTPure.Response
reportAndRenderErrorPage error = do
  let
    errorMsg =
      intercalate " "
        [ "An uncaught error occurred:"
        , fromMaybe (message error) $ stack error
        ]
  logError errorMsg
  debugMessage <- asks _.environment <#> case _ of
    Development -> Just errorMsg
    _ -> Nothing
  errorPage <- liftEffect
    $ Pug.renderFile "./static/pages/error.pug"
        { error:
            { message: "An error occurred on our end, sorry. We monitor these errors to fix them as fast as possible. Please try again later."
            , debugMessage
            }
        }
  html <- renderLayout errorPage
  HTTPure.internalServerError html

catchErrorMiddleware
  :: forall m
   . MonadError Error m
  => MonadAff m
  => Now m
  => LogMessages m
  => MonadAsk Env m
  => (HTTPure.Request -> m HTTPure.Response)
  -> HTTPure.Request
  -> m HTTPure.Response
catchErrorMiddleware router request =
  catchError (router request) reportAndRenderErrorPage

loggerMiddleware
  :: forall m
   . MonadAsk Env m
  => Now m
  => LogMessages m
  => MonadAff m
  => (HTTPure.Request -> m HTTPure.Response)
  -> HTTPure.Request
  -> m HTTPure.Response
loggerMiddleware router request = do
  let
    nowInMsInNumber =
      now <#> unInstant >>> un DurationTime.Milliseconds
  before <- nowInMsInNumber
  response <- router request
  after <- nowInMsInNumber
  let
    requestPath = case request.path of
      [] -> "/"
      parts -> intercalate "/" parts

    timeTaken = fold [ show $ Int.round $ after - before, "ms" ]
  logInfo $ intercalate " "
    [ show request.method # String.toUpper
    , requestPath
    , "-"
    , show response.status
    , timeTaken
    ]
  pure response

data Route
  = IndexRoute
  | CssRoute String
  | BlogRoute String
  | AssetsRoute String
  | NotFoundRoute String

requestToRoute :: HTTPure.Request -> Route
requestToRoute { path } = case path of
  [] -> IndexRoute
  [ "css", cssName ] -> CssRoute cssName
  [ "blog", unvalidatedSlug ] -> BlogRoute unvalidatedSlug
  other ->
    case List.fromFoldable other of
      "assets" : assetPath -> AssetsRoute $ intercalate "/" assetPath
      _ -> NotFoundRoute $ intercalate "/" other

routeHandler ::
  forall m
   . MonadAff m
  => MonadAsk Env m
  => LogMessages m
  => Now m
  => ReadBlogPosts m
  => RenderMarkdown m
  => RenderPug m
  => ReadAssets m
  => Route
  -> m HTTPure.Response
routeHandler = case _ of
  IndexRoute -> indexHandler
  CssRoute name -> cssHandler name
  BlogRoute unvalidatedSlug -> blogPostHandler unvalidatedSlug
  AssetsRoute assetPath -> fileHandler assetPath
  NotFoundRoute path -> respondWithError $ NotFound path

cssHandler
  :: forall m. MonadAff m => MonadAsk Env m => String -> m HTTPure.Response
cssHandler = renderStyles >>> handleStylesResult
  where
  handleStylesResult = case _ of
    Right cssBody -> HTTPure.ok cssBody
    Left (NotFound path) -> respondWithError (NotFound path)
    Left se -> respondWithError se

renderStyles :: String -> Either ServerError CssBody
renderStyles = case _ of
  "styles.css" ->
    CSS.render Styles.main
      # CSS.renderedSheet
          >>> fromMaybe'
            (\_ -> unsafeCrashWith "invalid CSS module Styles.main")
          >>> Body.cssBody
          >>> Right
  path -> Left $ NotFound path

fileHandler
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => ReadAssets m
  => FilePath
  -> m HTTPure.Response
fileHandler filePath =
  handleFileResult =<< (runExceptT $ fileRouter filePath)
  where
  handleFileResult :: Either ServerError AssetBody -> m HTTPure.Response
  handleFileResult = case _ of
    Left se -> respondWithError se
    Right assetBody' -> HTTPure.ok assetBody'

fileRouter
  :: forall m
   . ReadAssets m
  => FilePath
  -> ExceptT ServerError m AssetBody
fileRouter filePath = do
  let
    fullPath = Path.concat [ "./static/assets", filePath ]
  ExceptT $ note (NotFound filePath) <$> readAsset fullPath

indexHandler
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => ReadBlogPosts m
  => RenderPug m
  => m HTTPure.Response
indexHandler = renderIndex >>= HTTPure.ok

renderIndex
  :: forall m
   . MonadAsk Env m
  => ReadBlogPosts m
  => RenderPug m
  => m HtmlBody
renderIndex = do
  blogIndex <- readBlogIndex
  indexHtml <- renderPugFile "./static/pages/index.pug"
    { posts:
        Map.values blogIndex
          # List.sortBy (flip compare `on` BlogPost.timestamp)
    }
  renderLayout indexHtml

blogPostHandler
  :: forall m
   . MonadAsk Env m
  => MonadAff m
  => ReadBlogPosts m
  => RenderMarkdown m
  => RenderPug m
  => String
  -> m HTTPure.Response
blogPostHandler unvalidatedSlug =
  case Slug.fromString unvalidatedSlug of
    Nothing -> respondWithError $ InvalidSlug unvalidatedSlug
    Just slug -> do
      maybeBlogPost <- readBlogPost slug
      case maybeBlogPost of
        Nothing -> respondWithError $ NoBlogPostForSlug slug
        Just blogPost -> HTTPure.ok =<< renderBlogPost blogPost

renderBlogPost
  :: forall m
   . MonadAsk Env m
  => ReadBlogPosts m
  => RenderMarkdown m
  => RenderPug m
  => BlogPost
  -> m HtmlBody
renderBlogPost = do
  renderLayout <=< loadBlogPostHtmlForSlug

loadBlogPostHtmlForSlug
  :: forall m
   . ReadBlogPosts m
  => RenderMarkdown m
  => RenderPug m
  => BlogPost
  -> m HtmlBody
loadBlogPostHtmlForSlug blogPost = do
  let
    postMarkdown = BlogPost.content blogPost
  renderedMd <- renderMarkdown postMarkdown
  renderPugFile "./static/pages/blogpost.pug"
    { post: BlogPost.metaData blogPost
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
  | NoBlogPostForSlug Slug
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

-- TODO: Refactor and improve error handling
respondWithError
  :: forall m
   . MonadAsk Env m
  => MonadAff m
  => ServerError
  -> m HTTPure.Response
respondWithError serverError = do
  let
    error = serverErrorToErrorViewModel serverError
  errorHtml <- liftEffect $ Pug.renderFile "./static/pages/error.pug" { error }
  html <- renderLayout errorHtml
  HTTPure.response error.status html

serverErrorToErrorViewModel :: ServerError -> ErrorViewModel
serverErrorToErrorViewModel = case _ of
  InvalidSlug invalidSlug ->
    { status: 400
    , statusMessage: statusToString 400
    , message: "Invalid post slug: " <> invalidSlug
    }
  NotFound notFoundPath ->
    { status: 404
    , statusMessage: statusToString 404
    , message: "Not found: " <> notFoundPath
    }
  NoBlogPostForSlug slug ->
    { status: 404
    , statusMessage: statusToString 404
    , message: "No blog post found for slug " <> show slug
    }

appMMiddleware
  :: Environment
  -> (HTTPure.Request -> AppM HTTPure.Response)
  -> HTTPure.Request
  -> HTTPure.ResponseM
appMMiddleware environment router request = do
  renderHtmlWithTemplate <- liftEffect $ Pug.compileFile
    "./static/layout/main.pug"
  let
    env =
      { renderHtmlWithTemplate, environment }
    appM = router request
  runAppM appM env

main :: HTTPure.ServerM
main = do
  environment <- do
    maybeEnvironmentString <- Process.lookupEnv "ENV"
    case maybeEnvironmentString >>= parseEnvironment of
      Nothing -> do
        Console.warn "No \"ENV\" environment var set, defaulting to Production"
        pure Production
      Just env -> do
        Console.info $ intercalate " "
          [ "Environment set to:"
          , show env
          ]
        pure env
  HTTPure.serve 8080
    ( appMMiddleware environment
        $ loggerMiddleware
        $ catchErrorMiddleware
        $ (requestToRoute >>> routeHandler)
    )
    do
      Console.log
        "Server running on port 8080: http://localhost:8080"
