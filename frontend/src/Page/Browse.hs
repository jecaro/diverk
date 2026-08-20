module Page.Browse (page) where

import qualified Commonmark as Commonmark
import Control.Lens ((^.), (^?))
import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Control.Monad.Fix as MonadFix
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Aeson
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Either.Extra as Either
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.Lazy as LT
import qualified GHCJS.DOM.Types as GHCJSDOM
import qualified JSDOM.Element as JSDOM
import qualified JSDOM.Types as JSDOM
import qualified Model
import Reflex.Dom.Core
import Reflex.Extra (onClient)
import qualified Request
import qualified Route
import qualified Widget
import qualified Widget.Icon as Icon
import qualified Widget.Navbar as Navbar

data Error
  = ErStatus Word
  | ErJSON
  | ErBase64 Text.UnicodeException
  | ErMarkdown Commonmark.ParseError
  | ErRequest
  | ErInvalid
  deriving stock (Eq, Show)

data State
  = StInitial
  | StFetching
  | StDirectory [Model.Path]
  | StMarkdown (Commonmark.Html ())
  | StOther Text.Text
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
      v <- Either.maybeToEither ErJSON $ decodeXhrResponse response
      case v of
        Aeson.Array _ -> toDirectory v
        Aeson.Object _ -> toMarkdownOrCode v
        _ -> Left ErJSON
    code -> Left $ ErStatus code
  where
    toMarkdownOrCode :: Aeson.Value -> Either Error State
    toMarkdownOrCode v = do
      path <- Either.maybeToEither ErJSON $ parsePath v
      base64Content <- Either.maybeToEither ErJSON $ parseContent v
      rawContent <-
        Bifunctor.first ErBase64
          . Text.decodeUtf8'
          . Base64.decodeLenient
          $ Text.encodeUtf8 base64Content
      case extension path of
        "md" -> do
          parsed <-
            Bifunctor.first ErMarkdown $
              Commonmark.commonmark "markdown" rawContent
          pure $ StMarkdown parsed
        _ -> pure $ StOther rawContent

    toDirectory :: Aeson.Value -> Either Error State
    toDirectory =
      fmap StDirectory
        . Either.maybeToEither ErJSON
        . traverse toPath
        . Lens.toListOf Aeson.values

    toPath :: Aeson.Value -> Maybe Model.Path
    toPath = fmap Model.MkPath . parsePath

    extension =
      Text.takeWhileEnd (/= '.') . Maybe.fromMaybe "" . Lens.preview Lens._last
    parseContent =
      Lens.preview $ Aeson.key "content" . Aeson._String . Lens.to withoutEOL
    -- The GitHub API pads the text with newlines every 60 characters
    withoutEOL = Text.filter (/= '\n')
    parsePath =
      Lens.preview $ Aeson.key "path" . Aeson._String . Lens.to splitPath
    splitPath = Text.split (== '/')

errorToText :: Error -> Text.Text
errorToText (ErStatus code) = "Unexpected status code: " <> Text.pack (show code)
errorToText ErJSON = "Invalid JSON"
errorToText (ErBase64 err) = "Base64 error: " <> Text.pack (show err)
errorToText (ErMarkdown err) = "Markdown error: " <> Text.pack (show err)
errorToText ErRequest = "Request error"
errorToText ErInvalid = "Invalid state"

page ::
  ( DomBuilder t m,
    PostBuild t m,
    Prerender t m,
    MonadHold t m,
    MonadFix.MonadFix m,
    Route.Set t m,
    Route.ToUrl m,
    Route.Ask t m
  ) =>
  Model.Config ->
  [Text.Text] ->
  m ()
page Model.MkConfig {..} path = do
  evRequest <-
    (Request.contents coToken coOwner coRepo path <$) <$> getPostBuild
  evResponse <- onClient $ performRequestAsyncWithError evRequest

  dynState <-
    foldDyn updateState (Right StInitial) $
      leftmost
        [LoStartRequest <$ evRequest, LoEndRequest <$> evResponse]

  navbar' path $ Maybe.isJust coToken
  dyn_ . ffor dynState $ \case
    Left err -> Widget.error (errorToText err)
    Right state ->
      elClass "div" "flex flex-col gap-4 p-4 overflow-auto" $
        contentWidget state

contentWidget ::
  ( DomBuilder t m,
    Prerender t m,
    Route.Set t m,
    Route.ToUrl m
  ) =>
  State ->
  m ()
contentWidget (StDirectory pathsToFiles) =
  Monad.forM_ pathsToFiles $ \(Model.MkPath pathToFile) ->
    el "div" $
      Route.link (Route.Push (Route.Browse pathToFile)) $
        text . Maybe.fromMaybe "/" $
          pathToFile ^? Lens._last
contentWidget (StMarkdown html) =
  prerender_ blank $ do
    (e, _) <- elClass' "article" "prose" blank
    JSDOM.liftJSM $
      JSDOM.setInnerHTML
        (JSDOM.Element . GHCJSDOM.unElement $ _element_raw e)
        (LT.toStrict $ Commonmark.renderHtml html)
contentWidget (StOther code) =
  elClass "article" "prose" . el "pre" . el "code" . text $ code
contentWidget _ = Widget.spinner

navbar' ::
  ( DomBuilder t m,
    PostBuild t m,
    Route.Set t m,
    Route.ToUrl m,
    Route.Ask t m
  ) =>
  [Text.Text] ->
  Bool ->
  m ()
navbar' path hasToken =
  Navbar.widget $ do
    elClass "div" "breadcrumbs flex gap-x-4 w-full" $
      el "ul" $
        Foldable.traverse_ liIntermediatePath $
          List.inits path
    Navbar.menu hasToken
  where
    liIntermediatePath intermediatePath =
      el "li" $
        Route.link (Route.Push (Route.Browse intermediatePath)) $
          homeOrText intermediatePath
    homeOrText [] = Icon.house
    homeOrText [x] = text x
    homeOrText (_ : xs) = homeOrText xs
