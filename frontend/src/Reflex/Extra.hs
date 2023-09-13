module Reflex.Extra (onClient) where

import Reflex.Dom.Core

onClient ::
  forall a m t.
  (Applicative m, Prerender t m) =>
  Client m (Event t a) ->
  m (Event t a)
onClient = fmap switchDyn . prerender (pure never)
