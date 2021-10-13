module Main where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Control.Monad.Reader.Class (asks)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List ((:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.MarkdownIt as MD
import Foreign.Object as Obj
import Foreign.Pug as Pug
import HTTPure as HTTPure
import HTTPure.Status as HTTPureStatus
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path as Path

type Env =
  { renderHtmlWithTemplate :: Obj.Object String -> String
  }

readerMiddleware
  :: (HTTPure.Request -> ReaderT Env Aff HTTPure.Response)
  -> HTTPure.Request
  -> HTTPure.ResponseM
readerMiddleware router request = do
  renderHtmlWithTemplate <- liftEffect $ Pug.compileFile' "./static/layout/main.pug"
  let
    env = { renderHtmlWithTemplate }
  runReaderT (router request) env

indexRouter :: forall m. MonadAsk Env m => MonadAff m => HTTPure.Request -> m HTTPure.Response
indexRouter request = do
  case request.path of
    [] -> renderIndex
    [ "blog", slug ] -> renderBlogPost slug
    _ ->
      case List.fromFoldable request.path of
        "assets" : assetPath -> fileRouter "./static/assets" $ List.intercalate "/" assetPath
        _ -> renderNotFound

fileRouter :: forall m. MonadAff m => MonadAsk Env m => String -> String -> m HTTPure.Response
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

renderResource :: forall m. MonadAsk Env m => MonadAff m => String -> m HTTPure.Response
renderResource _ = HTTPure.internalServerError "Internal server error: Not yet implemented"

renderIndex :: forall m. MonadAsk Env m => MonadAff m => m HTTPure.Response
renderIndex = do
  renderHtmlWithTemplate <- asks _.renderHtmlWithTemplate
  indexContents <- liftEffect $ MD.renderString
    """
# This is the index page
working with markdown

a second paragraph?
    """
  okHtml $ renderHtmlWithTemplate $ Obj.fromHomogeneous { contents: indexContents }

renderBlogPost :: forall m. MonadAsk Env m => MonadAff m => String -> m HTTPure.Response
renderBlogPost slug = do
  blogPostMarkdown <- loadBlogPostHtmlForSlug slug
  renderHtmlWithTemplate <- asks _.renderHtmlWithTemplate
  okHtml $ renderHtmlWithTemplate $ Obj.fromHomogeneous { contents: blogPostMarkdown }
--HTTPure.ok $ "blog post for " <> slug

loadBlogPostHtmlForSlug :: forall m. MonadAff m => String -> m String
loadBlogPostHtmlForSlug slug = do
  -- TODO: Sanitize slug
  postMarkdown <- liftAff $ FS.readTextFile UTF8 $ "./static/blog/posts/" <> slug <> ".md"
  liftEffect $ MD.renderString postMarkdown

renderNotFound :: forall m. MonadAsk Env m => MonadAff m => m HTTPure.Response
renderNotFound = do
  renderHtmlWithTemplate <- asks _.renderHtmlWithTemplate
  HTTPure.response'
    HTTPureStatus.notFound
    (HTTPure.header "Content-Type" "text/html; charset=utf-8")
    $ renderHtmlWithTemplate
    $ Obj.fromHomogeneous { contents: "Not found" }

-- This can be made easier using the Body type class
okHtml :: forall m. MonadAff m => String -> m HTTPure.Response
okHtml html =
  HTTPure.ok' (HTTPure.header "Content-Type" "text/html; charset=utf-8") html

main :: HTTPure.ServerM
main = do
  HTTPure.serve 8080 (readerMiddleware indexRouter) $ Console.log "Server running on port 8080: http://localhost:8080"
