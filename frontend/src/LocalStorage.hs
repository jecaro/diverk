module LocalStorage (load, save) where

import Control.Lens ((^.))
import qualified Control.Lens as Lens
import Data.Functor (($>))
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified JSDOM.Storage.Extra as JSDOM
import qualified Language.Javascript.JSaddle as JSaddle
import qualified Model
import Reflex.Dom.Core
import Reflex.Extra (onClient)

load ::
  forall m t.
  ( Prerender t m,
    DomBuilder t m
  ) =>
  m (Event t (Maybe Model.Config))
load =
  onClient $ do
    ev <- getPostBuild
    performEvent
      ( ev
          $> JSaddle.liftJSM
            ( do
                mbOwner <- fmap Model.MkOwner <$> JSDOM.load ownerTag
                mbRepo <- fmap Model.MkRepo <$> JSDOM.load repoTag
                mbToken <- fmap Model.MkToken <$> JSDOM.load tokenTag
                darkMode' <- Maybe.fromMaybe False <$> JSDOM.load darkModeTag
                pure $
                  Model.MkConfig
                    <$> mbOwner
                    <*> mbRepo
                    <*> pure mbToken
                    <*> pure darkMode'
            )
      )

save ::
  forall m t.
  ( Prerender t m,
    Applicative m
  ) =>
  Event t Model.Config ->
  m (Event t Model.Config)
save ev =
  onClient . performEvent . ffor ev $ \config ->
    JSaddle.liftJSM $ do
      JSDOM.save ownerTag $ config ^. Model.owner . Lens._Wrapped
      JSDOM.save repoTag $ config ^. Model.repo . Lens._Wrapped
      case config ^. Model.token of
        Just token' -> JSDOM.save tokenTag $ token' ^. Lens._Wrapped
        Nothing -> JSDOM.clear tokenTag
      JSDOM.save darkModeTag $ config ^. Model.darkMode
      pure config

ownerTag :: Text.Text
ownerTag = "owner"

repoTag :: Text.Text
repoTag = "repo"

tokenTag :: Text.Text
tokenTag = "token"

darkModeTag :: Text.Text
darkModeTag = "dark"
