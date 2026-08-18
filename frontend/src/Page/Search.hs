{-# LANGUAGE CPP #-}

module Page.Search (page) where

import Model (Owner, Path (..), Repo, Token)
import Control.Arrow ((***))
import Control.Lens (to, toListOf, (^.), _Unwrapped)
import Control.Monad (join, when)
import Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as JSON
import Data.Aeson.Lens (key, values, _String)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHCJS.DOM.Types as GHCJSDOM
import JSDOM.Generated.HTMLElement (focus)
import qualified JSDOM.HTMLInputElement as JSDOM
import JSDOM.Types (liftJSM)
import Route (AskRoute, Nav (..), Route (..), RouteToUrl, SetRoute (..), routeLink)
import Reflex.Dom.Core hiding (Search)
import Reflex.Extra (onClient)
import qualified Request
import qualified Widget
import qualified Widget.Icon as Icon
import qualified Widget.Navbar as Navbar
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

page ::
  ( DomBuilder t m,
    PostBuild t m,
    Prerender t m,
    MonadHold t m,
    MonadFix m,
    SetRoute t m,
    RouteToUrl m,
    AskRoute t m
  ) =>
  Owner ->
  Repo ->
  Token ->
  [Text] ->
  m ()
page owner repo token keywords = do
  Navbar.widget $
    searchInput keywords >>= searchButton >> Navbar.menu True
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
      Right StFetching -> Widget.spinner
      Right (StResults []) -> el "div" $ text "No results"
      Right (StResults paths) -> traverse_ elPath paths
      Left err -> Widget.error $ errorToText err
  where
    request = Request.search token owner repo keywords
    elPath (MkPath pieces) =
      el "div" $
        routeLink (Browse pieces) $
          text $ T.intercalate "/" pieces

searchInput ::
  ( DomBuilder t m,
    Prerender t m,
    SetRoute t m
  ) =>
  [Text] ->
  m (Dynamic t [Text])
searchInput keywords = elClass "form-control" "flex-1" $ do
  (dyKeywords, evEnterOnNonEmptyKeywords) <- fmap unwrap . prerender (pure mempty) $
    do
      ie <- inputElement'
      -- Set focus on the input element after the page is loaded
      -- see: https://github.com/reflex-frp/reflex-dom/issues/435
      when (null keywords) $ do
        delayedPostBuild <- delay 0.1 =<< getPostBuild
        performEvent_ $
          liftJSM (focus $ htmlElement ie) <$ delayedPostBuild

      let dyKeywords = T.words <$> value ie
          evEnterOnNonEmptyKeywords =
            ffilter (not . null) . tagPromptlyDyn dyKeywords $ keypress Enter ie
      pure (dyKeywords, evEnterOnNonEmptyKeywords)
  setRoute $ Push . Search <$> evEnterOnNonEmptyKeywords
  pure dyKeywords
  where
    inputElement' =
      inputElement
        ( def
            & inputElementConfig_initialValue
            .~ T.unwords keywords
            & initialAttributes
            .~ ( "placeholder" =: "Keywords"
                   <> "type" =: "text"
                   <> "class" =: "input input-bordered w-full"
               )
        )
    unwrap = (join *** switchDyn) . splitDynPure
    htmlElement =
      JSDOM.HTMLInputElement . GHCJSDOM.unHTMLInputElement . _inputElement_raw

searchButton ::
  ( DomBuilder t m,
    PostBuild t m,
    SetRoute t m
  ) =>
  Dynamic t [Text] ->
  m ()
searchButton dyKeywords =
  elClass "label" "btn btn-ghost btn-circle" $
    dyn_ . ffor dyHasKeyWords $ \case
      True -> do
        (e, _) <- elDynClass' "span" (iconClasses <$> dyHasKeyWords) blank
        setRoute $ Push . Search <$> tagPromptlyDyn dyKeywords (domEvent Click e)
      False -> searchIcon
  where
    dyHasKeyWords = not . null <$> dyKeywords
    searchIcon = elDynClass "span" (iconClasses <$> dyHasKeyWords) blank
    iconClasses hasKw =
      T.unwords . mappend [Icon.solid, Icon.searchName] . pure $ opacity hasKw
    opacity True = mempty
    opacity False = "opacity-50"
