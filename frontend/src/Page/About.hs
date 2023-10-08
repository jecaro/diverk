module Page.About (page) where

import Common.Route (FrontendRoute)
import Control.Monad.Fix (MonadFix)
import Obelisk.Route.Frontend (R, RouteToUrl, Routed, SetRoute)
import Reflex.Dom.Core
import qualified Widget
import qualified Widget.Navbar as Navbar

page ::
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
page enableSearch = do
  Navbar.widget $
    Navbar.liSpacer >> Navbar.liMenu enableSearch
  Widget.card $ do
    elClass "div" "text-2xl font-bold" $
      text "Diverk"
    el "div" $ do
      el "div" $
        text "A minimalist GitHub repository browser"
      el "div" . Widget.link "https://github.com/jecaro/diverk" $
        text "Learn more"
