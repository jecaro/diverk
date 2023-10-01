module Browse (browse) where

import Common.Model (Config (..))
import Common.Route (FrontendRoute (..))
import qualified Commonmark as CM
import Control.Lens (preview, to, toListOf, (^.), (^?), _last)
import Control.Monad (forM_)
import Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as JSON
import Data.Aeson.Lens (key, values, _String)
import Data.Bifunctor (Bifunctor (first))
import Data.Either.Extra (maybeToEither)
import Data.List (inits)
import Data.Maybe (fromMaybe)
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
import Obelisk.Route.Frontend (RouteToUrl, SetRoute, routeLink, routeLinkAttr)
import Reflex.Dom.Core
import Reflex.Extra (onClient)
import Request (contentsRequest)

newtype PathToFile = MkPathToFile
  { unPathToFile :: [Text]
  }
  deriving stock (Eq, Show)

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
  | StDirectory [PathToFile]
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
        . traverse toPathToFile
        . toListOf values

    toPathToFile :: JSON.Value -> Maybe PathToFile
    toPathToFile = fmap MkPathToFile . parsePath

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

browse ::
  ( DomBuilder t m,
    PostBuild t m,
    Prerender t m,
    MonadHold t m,
    MonadFix m,
    SetRoute t (R FrontendRoute) m,
    RouteToUrl (R FrontendRoute) m
  ) =>
  Config ->
  [Text] ->
  m ()
browse MkConfig {..} path = do
  evRequest <-
    (contentsRequest coToken coOwner coRepo path <$) <$> getPostBuild
  evResponse <- onClient $ performRequestAsyncWithError evRequest

  dynState <-
    foldDyn updateState (Right StInitial) $
      leftmost
        [LoStartRequest <$ evRequest, LoEndRequest <$> evResponse]

  navbar path
  dyn_ . ffor dynState $ \case
    Left err -> errorWidget path err
    Right state ->
      elClass "div" "flex flex-col gap-4 p-4 overflow-auto" $
        contentWidget state

errorWidget ::
  ( RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    DomBuilder t m,
    Prerender t m
  ) =>
  [Text] ->
  Error ->
  m ()
errorWidget path err =
  elClass "div" "flex flex-col p-4" $
    elClass
      "div"
      ( T.unwords
          [ "p-4",
            "mx-auto",
            "text-red-800",
            "border",
            "border-red-300",
            "rounded-lg",
            "bg-red-50"
          ]
      )
      $ do
        elClass "div" "flex items-center" $ do
          elClass "i" "fa-solid fa-circle-info mr-2" blank
          elClass "h3" "text-lg font-medium" $
            text "An error occurred"
        elClass "div" "text-sm" $ do
          text $ errorToText err
          el "br" blank
          text "You can "
          routeLinkAttr
            ("class" =: "text-blue-600 hover:underline")
            (MkBrowse :/ path)
            (text "try again")

contentWidget ::
  ( SetRoute t (R FrontendRoute) m,
    RouteToUrl (R FrontendRoute) m,
    DomBuilder t m,
    Prerender t m
  ) =>
  State ->
  m ()
contentWidget (StDirectory pathsToFiles) =
  forM_ pathsToFiles $ \(MkPathToFile pathToFile) ->
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
contentWidget _ = spinner

navbar ::
  ( RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    DomBuilder t m,
    Prerender t m
  ) =>
  [Text] ->
  m ()
navbar path = do
  elClass "nav" "sticky shadow-md top-0 flex flex-col p-4 bg-white" $
    elClass "ol" "flex gap-x-4  w-full" $ do
      forM_ (inits path) $ \intermediatePath ->
        el "li" $
          routeLink (MkBrowse :/ intermediatePath) $ homeOrText intermediatePath
      -- spacer
      elClass "li" "grow" blank
      el "li" $ routeLink (MkConfiguration :/ ()) gear
  where
    house = elClass "i" "fa-solid fa-house" blank
    gear = elClass "i" "fa-solid fa-gear" blank
    homeOrText [] = house
    homeOrText [x] = text x
    homeOrText (_ : xs) = homeOrText xs

spinner :: DomBuilder t m => m ()
spinner =
  elClass
    "div"
    ( T.unwords
        [ "absolute",
          "right-1/2",
          "bottom-1/2",
          "transform",
          "translate-x-1/2",
          "translate-y-1/2"
        ]
    )
    $ elClass
      "div"
      ( T.unwords
          [ "border-t-transparent",
            "border-solid",
            "animate-spin",
            "rounded-full",
            "border-blue-400",
            "border-4",
            "h-8",
            "w-8"
          ]
      )
      blank
