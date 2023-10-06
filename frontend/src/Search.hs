module Search (search) where

import Common.Model (Owner, Path (..), Repo, Token)
import Common.Route (FrontendRoute (MkBrowse, MkSearch))
import Control.Lens (to, toListOf, (^.), _Unwrapped)
import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as JSON
import Data.Aeson.Lens (key, values, _String)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import JSDOM.Generated.HTMLElement (focus)
import JSDOM.Types (liftJSM)
import Navbar (liMenu, navbar)
import Obelisk.Route.Frontend
  ( R,
    RouteToUrl,
    Routed,
    SetRoute,
    dynRouteLink,
    routeLink,
    pattern (:/),
  )
import Reflex.Dom.Core
import Reflex.Extra (onClient)
import Request (searchRequest)
import Widgets (errorWidget, spinner)
import qualified Witherable as W

data Error
  = ErStatus Word
  | ErJSON
  | ErRequest
  | ErInvalid
  deriving stock (Eq, Show)

data State
  = StInitial
  | StFetching
  | StResults [Path]
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
    200 ->
      maybe
        (Left ErJSON)
        (Right . StResults . toPaths)
        $ decodeXhrResponse response
    code -> Left $ ErStatus code
  where
    toPaths :: JSON.Value -> [Path]
    toPaths =
      toListOf $
        key "items"
          . values
          . key "path"
          . _String
          . to (T.splitOn "/")
          . _Unwrapped

errorToText :: Error -> Text
errorToText (ErStatus code) = "Unexpected status code: " <> T.pack (show code)
errorToText ErJSON = "Invalid JSON"
errorToText ErRequest = "Request error"
errorToText ErInvalid = "Invalid state"

search ::
  ( DomBuilder t m,
    PostBuild t m,
    Prerender t m,
    MonadHold t m,
    SetRoute t (R FrontendRoute) m,
    RouteToUrl (R FrontendRoute) m,
    MonadFix m,
    Routed t (R FrontendRoute) m
  ) =>
  Owner ->
  Repo ->
  Token ->
  [Text] ->
  m ()
search owner repo token keywords = do
  navbar $
    liSearchInput keywords >>= liButton >> liMenu True
  elClass "div" "flex flex-col gap-4 p-4 overflow-auto" $ do
    -- We dont send the request if there is no keywords
    evRequest <-
      (request <$) . W.filter (const . not $ null keywords) <$> getPostBuild
    evResponse <- onClient $ performRequestAsyncWithError evRequest
    dyState <-
      foldDyn updateState (Right StInitial) $
        leftmost
          [ LoStartRequest <$ evRequest,
            LoEndRequest <$> evResponse
          ]
    dyn_ . ffor dyState $ \case
      Right StInitial -> blank
      Right StFetching -> spinner
      Right (StResults []) -> el "div" $ text "No results"
      Right (StResults paths) -> traverse_ elPath paths
      Left err -> errorWidget $ errorToText err
  where
    request = searchRequest token owner repo keywords
    elPath (MkPath pieces) =
      el "div" $
        routeLink (MkBrowse :/ pieces) . text $ T.intercalate "/" pieces

liSearchInput ::
  ( SetRoute t (R FrontendRoute) m,
    PostBuild t m,
    Prerender t m,
    DomBuilder t m
  ) =>
  [Text] ->
  m (Dynamic t [Text])
liSearchInput keywords = elClass "li" "grow" $
  fmap join . prerender (pure mempty) $ do
    ie <-
      inputElement
        ( def
            & inputElementConfig_initialValue
            .~ T.unwords keywords
            & initialAttributes
            .~ ( "placeholder" =: "Keywords"
                   <> "class"
                     =: T.unwords
                       [ "pl-0",
                         "h-6",
                         "w-full",
                         "grow",
                         "border-none",
                         "focus:outline-none",
                         "focus:ring-0",
                         "text-gray-900"
                       ]
               )
        )
    -- Set focus on the input element after the page is loaded
    -- see: https://github.com/reflex-frp/reflex-dom/issues/435
    delayedPostBuild <- delay 0.1 =<< getPostBuild
    performEvent_ $
      liftJSM (focus $ _inputElement_raw ie) <$ delayedPostBuild
    pure $ T.words <$> value ie

liButton ::
  ( RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    PostBuild t m,
    Prerender t m,
    DomBuilder t m
  ) =>
  Dynamic t [Text] ->
  m ()
liButton dyKeywords =
  el "li" $
    dyn_ . ffor dyHasKeyWords $ \case
      True -> dynRouteLink (searchRoute <$> dyKeywords) searchIcon
      False -> searchIcon
  where
    dyHasKeyWords = not . null <$> dyKeywords
    searchIcon = elDynClass "i" (iconClasses <$> dyHasKeyWords) blank
    iconClasses =
      T.unwords . mappend ["fa-solid", "fa-magnifying-glass"] . pure . opacity
    opacity True = mempty
    opacity False = "opacity-50"
    searchRoute = (MkSearch :/)
