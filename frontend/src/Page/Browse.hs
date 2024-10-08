module Page.Browse (page) where

import Common.Model (Config (..), Path (..))
import Common.Route (FrontendRoute (..))
import qualified Commonmark as CM
import Control.Lens (preview, to, toListOf, (^.), (^?), _last)
import Control.Monad (forM_)
import Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as JSON
import Data.Aeson.Lens (key, values, _String)
import Data.Bifunctor (Bifunctor (first))
import Data.Either.Extra (maybeToEither)
import Data.Foldable (traverse_)
import Data.List (inits)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Text.Encoding.Base64 (decodeBase64With)
import Data.Text.Encoding.Base64.Error (Base64Error)
import Data.Text.Encoding.Error (UnicodeException)
import qualified Data.Text.Lazy as LT
import qualified GHCJS.DOM.Types as GHCJSDOM
import JSDOM.Element (setInnerHTML)
import qualified JSDOM.Element as JSDOM
import JSDOM.Types (liftJSM)
import Obelisk.Route (R, pattern (:/))
import Obelisk.Route.Frontend (RouteToUrl, Routed, SetRoute, routeLink)
import Reflex.Dom.Core
import Reflex.Extra (onClient)
import qualified Request
import qualified Widget
import qualified Widget.Icon as Icon
import qualified Widget.Navbar as Navbar

data Error
  = ErStatus Word
  | ErJSON
  | ErBase64 (Base64Error UnicodeException)
  | ErMarkdown CM.ParseError
  | ErRequest
  | ErInvalid
  deriving stock (Eq, Show)

data State
  = StInitial
  | StFetching
  | StDirectory [Path]
  | StMarkdown (CM.Html ())
  | StOther Text
  deriving stock (Show)

data LocalEvent
  = LoStartRequest
  | LoEndRequest (Either XhrException XhrResponse)

updateState :: LocalEvent -> Either Error State -> Either Error State
updateState LoStartRequest (Right StInitial) = Right StFetching
updateState (LoEndRequest (Left _)) (Right StFetching) = Left ErRequest
updateState (LoEndRequest (Right response)) (Right StFetching) =
  responseToState response
updateState _ _ = Left ErInvalid

responseToState :: XhrResponse -> Either Error State
responseToState response =
  case response ^. xhrResponse_status of
    200 -> do
      v <- maybeToEither ErJSON $ decodeXhrResponse response
      case v of
        JSON.Array _ -> toDirectory v
        JSON.Object _ -> toMarkdownOrCode v
        _ -> Left ErJSON
    code -> Left $ ErStatus code
  where
    toMarkdownOrCode :: JSON.Value -> Either Error State
    toMarkdownOrCode v = do
      path <- maybeToEither ErJSON $ parsePath v
      base64Content <- maybeToEither ErJSON $ parseContent v
      rawContent <-
        first ErBase64
          . decodeBase64With decodeUtf8'
          $ encodeUtf8 base64Content
      case extension path of
        "md" -> do
          parsed <- first ErMarkdown $ CM.commonmark "markdown" rawContent
          pure $ StMarkdown parsed
        _ -> pure $ StOther rawContent

    toDirectory :: JSON.Value -> Either Error State
    toDirectory =
      fmap StDirectory
        . maybeToEither ErJSON
        . traverse toPath
        . toListOf values

    toPath :: JSON.Value -> Maybe Path
    toPath = fmap MkPath . parsePath

    extension = T.takeWhileEnd (/= '.') . fromMaybe "" . preview _last
    parseContent = preview $ key "content" . _String . to withoutEOL
    -- The GitHub API pads the text with newlines every 60 characters
    withoutEOL = T.filter (/= '\n')
    parsePath = preview $ key "path" . _String . to splitPath
    splitPath = T.split (== '/')

errorToText :: Error -> Text
errorToText (ErStatus code) = "Unexpected status code: " <> T.pack (show code)
errorToText ErJSON = "Invalid JSON"
errorToText (ErBase64 err) = "Base64 error: " <> T.pack (show err)
errorToText (ErMarkdown err) = "Markdown error: " <> T.pack (show err)
errorToText ErRequest = "Request error"
errorToText ErInvalid = "Invalid state"

page ::
  ( DomBuilder t m,
    PostBuild t m,
    Prerender t m,
    MonadHold t m,
    MonadFix m,
    SetRoute t (R FrontendRoute) m,
    RouteToUrl (R FrontendRoute) m,
    Routed t (R FrontendRoute) m
  ) =>
  Config ->
  [Text] ->
  m ()
page MkConfig {..} path = do
  evRequest <-
    (Request.contents coToken coOwner coRepo path <$) <$> getPostBuild
  evResponse <- onClient $ performRequestAsyncWithError evRequest

  dynState <-
    foldDyn updateState (Right StInitial) $
      leftmost
        [LoStartRequest <$ evRequest, LoEndRequest <$> evResponse]

  navbar' path (isJust coToken)
  dyn_ . ffor dynState $ \case
    Left err -> Widget.error (errorToText err)
    Right state ->
      elClass "div" "flex flex-col gap-4 p-4 overflow-auto" $
        contentWidget state

contentWidget ::
  ( SetRoute t (R FrontendRoute) m,
    RouteToUrl (R FrontendRoute) m,
    DomBuilder t m,
    Prerender t m
  ) =>
  State ->
  m ()
contentWidget (StDirectory pathsToFiles) =
  forM_ pathsToFiles $ \(MkPath pathToFile) ->
    el "div" $
      routeLink (MkBrowse :/ pathToFile)
        . text
        . fromMaybe "/"
        $ pathToFile ^? _last
contentWidget (StMarkdown html) =
  prerender_ blank $ do
    (e, _) <- elClass' "article" "prose" blank
    liftJSM $
      setInnerHTML
        (JSDOM.Element . GHCJSDOM.unElement $ _element_raw e)
        (LT.toStrict $ CM.renderHtml html)
contentWidget (StOther code) =
  elClass "article" "prose" . el "pre" . el "code" . text $ code
contentWidget _ = Widget.spinner

navbar' ::
  ( RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    DomBuilder t m,
    Prerender t m,
    Routed t (R FrontendRoute) m,
    PostBuild t m
  ) =>
  [Text] ->
  Bool ->
  m ()
navbar' path hasToken =
  Navbar.widget $ do
    elClass "div" "breadcrumbs flex gap-x-4 w-full" $
      el "ul" $ traverse_ liIntermediatePath (inits path)
    Navbar.menu hasToken
  where
    liIntermediatePath intermediatePath =
      el "li" . routeLink (MkBrowse :/ intermediatePath) $
        homeOrText intermediatePath
    homeOrText [] = Icon.house
    homeOrText [x] = text x
    homeOrText (_ : xs) = homeOrText xs
