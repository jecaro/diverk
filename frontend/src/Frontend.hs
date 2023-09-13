{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend (frontend) where

import Common.Model (Owner (MkOwner, unOwner), Repo (MkRepo, unRepo))
import Common.Route (FrontendRoute (..))
import Configuration (configuration)
import Control.Arrow ((***))
import Control.Lens (both)
import Control.Monad.Fix (MonadFix)
import Data.Bitraversable (bisequence)
import Data.Functor (($>))
import Language.Javascript.JSaddle (liftJSM)
import LocalStorage (load, save)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Generated.Static (static)
import Obelisk.Route (R, pattern (:/))
import Obelisk.Route.Frontend (RouteToUrl, Routed, SetRoute (..), askRoute)
import Reflex.Dom.Core
import Reflex.Extra (onClient)
import Tree (tree)

data State
  = -- | The initial state: before the config is loaded from the local storage
    MkInit
  | -- | After the config is loaded from the local storage
    MkConfigLoaded (Maybe (Owner, Repo))
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
    "link"
    ( "href" =: $(static "css/styles.css")
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
frontendBody =
  elAttr "div" ("class" =: "mt-4 mb-4 mr-4 ml-4 space-y-4") $ do
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
  evOk <- configuration mbConfig
  evSaved <- setToLocalStorage evOk
  setRoute $ MkBrowse :/ [] <$ evSaved
  pure $ MkConfigLoaded . Just <$> evSaved
route (MkBrowse :/ path) (MkConfigLoaded (Just (owner, repo))) = do
  tree owner repo path
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
  m (Event t (Maybe (Owner, Repo)))
getFromLocalStorage =
  onClient $ do
    ev <- getPostBuild
    performEvent
      ( ev
          $> liftJSM
            ( do
                mbTupleOfText <-
                  bisequence
                    <$> both load ("owner", "repo")
                pure $ (MkOwner *** MkRepo) <$> mbTupleOfText
            )
      )

setToLocalStorage ::
  forall m t.
  ( Prerender t m,
    Applicative m
  ) =>
  Event t (Owner, Repo) ->
  m (Event t (Owner, Repo))
setToLocalStorage ev =
  onClient $
    performEvent
      ( ffor ev $ \(owner, repo) ->
          liftJSM
            ( do
                save "owner" $ unOwner owner
                save "repo" $ unRepo repo
                pure (owner, repo)
            )
      )
