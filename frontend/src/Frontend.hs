{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend (frontend) where

import Common.Model
  ( Config (..),
    Owner (..),
    Repo (..),
    Token (..),
    owner,
    repo,
    token,
  )
import Common.Route (FrontendRoute (..))
import Configuration (configuration)
import Control.Lens ((^.), (^?), _Wrapped, _last)
import Control.Monad (forM_)
import Control.Monad.Fix (MonadFix)
import Data.Functor (($>))
import Data.List (inits)
import Language.Javascript.JSaddle (liftJSM)
import LocalStorage (clear, load, save)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Generated.Static (static)
import Obelisk.Route (R, pattern (:/))
import Obelisk.Route.Frontend
  ( RouteToUrl,
    Routed,
    SetRoute (..),
    askRoute,
    routeLink,
  )
import Reflex.Dom.Core
import Reflex.Extra (onClient)
import Tree (tree)

data State
  = -- | The initial state: before the config is loaded from the local storage
    MkInit
  | -- | After the config is loaded from the local storage
    MkConfigLoaded (Maybe Config)
  deriving stock (Show, Eq)

frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = frontendHead,
      _frontend_body = frontendBody
    }

frontendHead :: DomBuilder t m => m ()
frontendHead = do
  el "title" $ text "Diverk"
  elAttr
    "meta"
    ( "name" =: "viewport"
        <> "contents" =: "width=device-width, initial-scale=1.0"
    )
    blank

  elAttr
    "link"
    ( "href" =: $(static "css/styles.css")
        <> "type" =: "text/css"
        <> "rel" =: "stylesheet"
    )
    blank
  elAttr
    "link"
    ( "href" =: $(static "fontawesome/css/all.css")
        <> "type" =: "text/css"
        <> "rel" =: "stylesheet"
    )
    blank

frontendBody ::
  forall t m.
  ( DomBuilder t m,
    Prerender t m,
    Routed t (R FrontendRoute) m,
    MonadFix m,
    MonadHold t m,
    PostBuild t m,
    SetRoute t (R FrontendRoute) m,
    RouteToUrl (R FrontendRoute) m
  ) =>
  m ()
frontendBody = do
  evSettingsLoaded <- fmap MkConfigLoaded <$> getFromLocalStorage
  dyRoute <- askRoute

  rec dyState <-
        holdDyn MkInit $ leftmost [evSettingsLoaded, evSettingsSaved]
      evSettingsSaved <-
        switchHold never =<< dyn (route <$> dyRoute <*> dyState)

  pure ()

route ::
  ( DomBuilder t m,
    Prerender t m,
    SetRoute t (R FrontendRoute) m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    RouteToUrl (R FrontendRoute) m
  ) =>
  R FrontendRoute ->
  State ->
  m (Event t State)
route (MkConfiguration :/ ()) (MkConfigLoaded mbConfig) = do
  elAttr "div" ("class" =: "flex flex-col gap-4 p-4") $ do
    evOk <- configuration mbConfig
    evSaved <- setToLocalStorage evOk
    setRoute $ MkBrowse :/ [] <$ evSaved
    pure $ MkConfigLoaded . Just <$> evSaved
route (MkBrowse :/ path) (MkConfigLoaded (Just config)) = do
  elAttr "nav" ("class" =: "sticky shadow-md top-0 flex flex-col p-4 bg-white") $
    elAttr "ol" ("class" =: "flex gap-x-4  w-full") $ do
      forM_ (inits path) $ \partialPath ->
        el "li" $ do
          let homeOrText =
                maybe
                  (elAttr "i" ("class" =: "fa-solid fa-house") blank)
                  text
                  $ partialPath ^? _last
          routeLink (MkBrowse :/ partialPath) homeOrText
      elAttr "li" ("class" =: "grow") blank
      el "li" $
        routeLink (MkConfiguration :/ ()) $
          elAttr "i" ("class" =: "fa-solid fa-gear") blank
  elAttr
    "div"
    ("class" =: "flex flex-col gap-4 p-4 overflow-y-scroll")
    $ tree config path
  pure never
route (MkHome :/ ()) (MkConfigLoaded (Just _)) = do
  setRoute . (MkBrowse :/ [] <$) =<< getPostBuild
  pure never
route _ (MkConfigLoaded Nothing) = do
  setRoute . (MkConfiguration :/ () <$) =<< getPostBuild
  pure never
route _ _ = pure never

getFromLocalStorage ::
  forall m t.
  ( Prerender t m,
    DomBuilder t m
  ) =>
  m (Event t (Maybe Config))
getFromLocalStorage =
  onClient $ do
    ev <- getPostBuild
    performEvent
      ( ev
          $> liftJSM
            ( do
                mbOwner <- fmap MkOwner <$> load "owner"
                mbRepo <- fmap MkRepo <$> load "repo"
                mbToken <- fmap MkToken <$> load "token"
                pure $ MkConfig <$> mbOwner <*> mbRepo <*> pure mbToken
            )
      )

setToLocalStorage ::
  forall m t.
  ( Prerender t m,
    Applicative m
  ) =>
  Event t Config ->
  m (Event t Config)
setToLocalStorage ev =
  onClient $
    performEvent
      ( ffor ev $ \config ->
          liftJSM
            ( do
                save "owner" $ config ^. owner . _Wrapped
                save "repo" $ config ^. repo . _Wrapped
                case config ^. token of
                  Just token' -> save "token" $ token' ^. _Wrapped
                  Nothing -> clear "token"
                pure config
            )
      )
