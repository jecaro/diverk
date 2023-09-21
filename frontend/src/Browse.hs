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
import Data.Either.Extra (fromEither, maybeToEither)
import Data.List (inits)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Text.Encoding.Base64 (decodeBase64With)
import Data.Text.Encoding.Base64.Error (Base64Error)
import Data.Text.Encoding.Error (UnicodeException)
import qualified Data.Text.Lazy as LT
import JSDOM.Element (setInnerHTML)
import JSDOM.Types (liftJSM)
import Obelisk.Route (R, pattern (:/))
import Obelisk.Route.Frontend (RouteToUrl, SetRoute, routeLink)
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
  | StError Error
  deriving stock (Show)

data LocalEvent
  = LoStartRequest
  | LoEndRequest (Either XhrException XhrResponse)

updateState :: LocalEvent -> State -> State
updateState LoStartRequest StInitial = StFetching
updateState (LoEndRequest (Left _)) StFetching = StError ErRequest
updateState (LoEndRequest (Right response)) StFetching = responseToState response
updateState _ _ = StError ErInvalid

responseToState :: XhrResponse -> State
responseToState response =
  case response ^. xhrResponse_status of
    200 -> fromEither $ first StError eiObjects
    code -> StError $ ErStatus code
  where
    eiObjects :: Either Error State
    eiObjects = do
      v <- maybeToEither ErJSON $ decodeXhrResponse response
      case v of
        JSON.Array _ -> toDirectory v
        JSON.Object _ -> toMarkdownOrCode v
        _ -> Left ErJSON

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
browse config path = do
  navbar path
  contentWidget config path

navbar ::
  ( RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    DomBuilder t m,
    Prerender t m
  ) =>
  [Text] ->
  m ()
navbar path = do
  elAttr "nav" ("class" =: "sticky shadow-md top-0 flex flex-col p-4 bg-white") $
    elAttr "ol" ("class" =: "flex gap-x-4  w-full") $ do
      forM_ (inits path) $ \intermediatePath ->
        el "li" $
          routeLink (MkBrowse :/ intermediatePath) $ homeOrText intermediatePath
      -- spacer
      elAttr "li" ("class" =: "grow") blank
      el "li" $ routeLink (MkConfiguration :/ ()) gear
  where
    house = elAttr "i" ("class" =: "fa-solid fa-house") blank
    gear = elAttr "i" ("class" =: "fa-solid fa-gear") blank
    homeOrText [] = house
    homeOrText [x] = text x
    homeOrText (_ : xs) = homeOrText xs

contentWidget ::
  ( RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    Prerender t m
  ) =>
  Config ->
  [Text] ->
  m ()
contentWidget MkConfig {..} path = do
  evRequest <-
    (contentsRequest coToken coOwner coRepo path <$) <$> getPostBuild
  evResponse <- onClient $ performRequestAsyncWithError evRequest

  dynState <-
    foldDyn updateState StInitial $
      leftmost
        [LoStartRequest <$ evRequest, LoEndRequest <$> evResponse]

  elAttr "div" ("class" =: "flex flex-col gap-4 p-4 overflow-auto") $
    dyn_ . ffor dynState $ \case
      StError err -> el "div" . text . T.pack $ show err
      StDirectory pathsToFiles ->
        forM_ pathsToFiles $ \(MkPathToFile pathToFile) ->
          el "div" $
            routeLink (MkBrowse :/ pathToFile)
              . text
              . fromMaybe "/"
              $ pathToFile ^? _last
      StMarkdown html ->
        prerender_ blank $ do
          (e, _) <- elAttr' "article" ("class" =: "prose") blank
          liftJSM $
            setInnerHTML (_element_raw e) (LT.toStrict $ CM.renderHtml html)
      StOther code ->
        elAttr "article" ("class" =: "prose") $
          el "pre" . el "code" . text $ code
      _ -> blank
