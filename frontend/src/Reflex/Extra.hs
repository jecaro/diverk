module Reflex.Extra (getGlobalClick, onClient) where

import qualified JSDOM as DOM
import qualified JSDOM.EventM as EventM
import qualified JSDOM.Generated.GlobalEventHandlers as Events
import Reflex.Dom.Core

onClient ::
  forall a m t.
  (Applicative m, Prerender t m) =>
  Client m (Event t a) ->
  m (Event t a)
onClient = fmap switchDyn . prerender (pure never)

getGlobalClick :: (DomBuilder t m, Prerender t m) => m (Event t ())
getGlobalClick = onClient $ do
  document <- DOM.currentDocumentUnchecked
  wrapDomEvent document (`EventM.on` Events.click) blank
