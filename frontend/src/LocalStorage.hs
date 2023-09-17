module LocalStorage (load, save) where

import Common.Model
  ( Config (..),
    Owner (..),
    Repo (..),
    Token (..),
    owner,
    repo,
    token,
  )
import Control.Lens ((^.), _Wrapped)
import Data.Functor (($>))
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
                mbOwner <- fmap MkOwner <$> JSDOM.load "owner"
                mbRepo <- fmap MkRepo <$> JSDOM.load "repo"
                mbToken <- fmap MkToken <$> JSDOM.load "token"
                pure $ MkConfig <$> mbOwner <*> mbRepo <*> pure mbToken
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
  onClient $
    performEvent
      ( ffor ev $ \config ->
          liftJSM
            ( do
                JSDOM.save "owner" $ config ^. owner . _Wrapped
                JSDOM.save "repo" $ config ^. repo . _Wrapped
                case config ^. token of
                  Just token' -> JSDOM.save "token" $ token' ^. _Wrapped
                  Nothing -> JSDOM.clear "token"
                pure config
            )
      )
