module About (about) where

import Common.Route (FrontendRoute)
import Control.Monad.Fix (MonadFix)
import Navbar (liMenu, liSpacer, navbar)
import Obelisk.Route.Frontend (R, RouteToUrl, Routed, SetRoute)
import Reflex.Dom.Core
import Widgets (card, elLink)

about ::
  ( DomBuilder t m,
    SetRoute t (R FrontendRoute) m,
    RouteToUrl (R FrontendRoute) m,
    Routed t (R FrontendRoute) m,
    MonadFix m,
    MonadHold t m,
    PostBuild t m,
    Prerender t m
  ) =>
  Bool ->
  m ()
about enableSearch = do
  navbar $
    liSpacer >> liMenu enableSearch
  card $ do
    elClass "div" "text-2xl font-bold" $
      text "Diverk"
    el "div" $ do
      el "div" $
        text "A minimalist GitHub repository browser"
      el "div" . elLink "https://github.com/jecaro/diverk" $
        text "Learn more"
