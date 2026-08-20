{-# LANGUAGE CPP #-}

module Page.Search (page) where

import Control.Arrow ((***))
import Control.Lens ((^.))
import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Control.Monad.Fix as MonadFix
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Lens as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified GHCJS.DOM.Types as GHCJSDOM
import qualified JSDOM.Generated.HTMLElement as JSDOM
import qualified JSDOM.HTMLInputElement as JSDOM
import qualified JSDOM.Types as JSDOM
import qualified Model
import Reflex.Dom.Core hiding (Search)
import Reflex.Extra (onClient)
import qualified Request
import qualified Route
import qualified Widget
import qualified Widget.Icon as Icon
import qualified Widget.Navbar as Navbar
import qualified Witherable

data Error
  = ErStatus Word
  | ErJSON
  | ErRequest
  | ErInvalid
  deriving stock (Eq, Show)

data State
  = StInitial
  | StFetching
  | StResults [Model.Path]
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
    toPaths :: JSON.Value -> [Model.Path]
    toPaths =
      Lens.toListOf $
        Aeson.key "items"
          . Aeson.values
          . Aeson.key "path"
          . Aeson._String
          . Lens.to (Text.splitOn "/")
          . Lens._Unwrapped

errorToText :: Error -> Text.Text
errorToText (ErStatus code) = "Unexpected status code: " <> Text.pack (show code)
errorToText ErJSON = "Invalid JSON"
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
  Model.Owner ->
  Model.Repo ->
  Model.Token ->
  [Text.Text] ->
  m ()
page owner repo token keywords = do
  Navbar.widget $
    searchInput keywords >>= searchButton >> Navbar.menu True
  elClass "div" "flex flex-col gap-4 p-4 overflow-auto" $ do
    -- We dont send the request if there is no keywords
    evRequest <-
      (request <$) . Witherable.filter (const . not $ null keywords) <$> getPostBuild
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
      Right (StResults paths) -> Foldable.traverse_ elPath paths
      Left err -> Widget.error $ errorToText err
  where
    request = Request.search token owner repo keywords
    elPath (Model.MkPath pieces) =
      el "div" $
        Route.link (Route.Push $ Route.Browse pieces) $
          text $
            Text.intercalate "/" pieces

searchInput ::
  ( DomBuilder t m,
    Prerender t m,
    Route.Set t m
  ) =>
  [Text.Text] ->
  m (Dynamic t [Text.Text])
searchInput keywords = elClass "form-control" "flex-1" $ do
  (dyKeywords, evEnterOnNonEmptyKeywords) <- fmap unwrap . prerender (pure mempty) $
    do
      ie <- inputElement'
      -- Set focus on the input element after the page is loaded
      -- see: https://github.com/reflex-frp/reflex-dom/issues/435
      Monad.when (null keywords) $ do
        delayedPostBuild <- delay 0.1 =<< getPostBuild
        performEvent_ $
          JSDOM.liftJSM (JSDOM.focus $ htmlElement ie) <$ delayedPostBuild

      let dyKeywords = Text.words <$> value ie
          evEnterOnNonEmptyKeywords =
            ffilter (not . null) . tagPromptlyDyn dyKeywords $ keypress Enter ie
      pure (dyKeywords, evEnterOnNonEmptyKeywords)
  Route.set $ Route.Push . Route.Search <$> evEnterOnNonEmptyKeywords
  pure dyKeywords
  where
    inputElement' =
      inputElement
        ( def
            & inputElementConfig_initialValue
            .~ Text.unwords keywords
            & initialAttributes
            .~ ( "placeholder" =: "Keywords"
                   <> "type" =: "text"
                   <> "class" =: "input input-bordered w-full"
               )
        )
    unwrap = (Monad.join *** switchDyn) . splitDynPure
    htmlElement =
      JSDOM.HTMLInputElement . GHCJSDOM.unHTMLInputElement . _inputElement_raw

searchButton ::
  ( DomBuilder t m,
    PostBuild t m,
    Route.Set t m
  ) =>
  Dynamic t [Text.Text] ->
  m ()
searchButton dyKeywords =
  elClass "label" "btn btn-ghost btn-circle" $
    dyn_ . ffor dyHasKeyWords $ \case
      True -> do
        (e, _) <- elDynClass' "span" (iconClasses <$> dyHasKeyWords) blank
        Route.set $ Route.Push . Route.Search <$> tagPromptlyDyn dyKeywords (domEvent Click e)
      False -> searchIcon
  where
    dyHasKeyWords = not . null <$> dyKeywords
    searchIcon = elDynClass "span" (iconClasses <$> dyHasKeyWords) blank
    iconClasses hasKw =
      Text.unwords . mappend [Icon.solid, Icon.searchName] . pure $ opacity hasKw
    opacity True = mempty
    opacity False = "opacity-50"
