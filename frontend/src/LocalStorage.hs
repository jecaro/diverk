module LocalStorage (load, save) where

import Common.Model
  ( Config (..),
    Owner (..),
    Repo (..),
    Token (..),
    darkMode,
    owner,
    repo,
    token,
  )
import Control.Lens ((^.), _Wrapped)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified JSDOM.Storage.Extra as JSDOM
import Language.Javascript.JSaddle (liftJSM)
import Reflex.Dom.Core
import Reflex.Extra (onClient)

load ::
  forall m t.
  ( Prerender t m,
    DomBuilder t m
  ) =>
  m (Event t (Maybe Config))
load =
  onClient $ do
    ev <- getPostBuild
    performEvent
      ( ev
          $> liftJSM
            ( do
                mbOwner <- fmap MkOwner <$> JSDOM.load ownerTag
                mbRepo <- fmap MkRepo <$> JSDOM.load repoTag
                mbToken <- fmap MkToken <$> JSDOM.load tokenTag
                darkMode' <- fromMaybe False <$> JSDOM.load darkModeTag
                pure $
                  MkConfig
                    <$> mbOwner <*> mbRepo <*> pure mbToken <*> pure darkMode'
            )
      )

save ::
  forall m t.
  ( Prerender t m,
    Applicative m
  ) =>
  Event t Config ->
  m (Event t Config)
save ev =
  onClient . performEvent . ffor ev $ \config ->
    liftJSM $ do
      JSDOM.save ownerTag $ config ^. owner . _Wrapped
      JSDOM.save repoTag $ config ^. repo . _Wrapped
      case config ^. token of
        Just token' -> JSDOM.save tokenTag $ token' ^. _Wrapped
        Nothing -> JSDOM.clear tokenTag
      JSDOM.save darkModeTag $ config ^. darkMode
      pure config

ownerTag :: Text
ownerTag = "owner"

repoTag :: Text
repoTag = "repo"

tokenTag :: Text
tokenTag = "token"

darkModeTag :: Text
darkModeTag = "dark"
