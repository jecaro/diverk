module Page.About (page) where

import Common.Route (FrontendRoute)
import Obelisk.Route.Frontend (R, Routed, SetRoute)
import Reflex.Dom.Core
import qualified Widget
import qualified Widget.Navbar as Navbar

page ::
  ( DomBuilder t m,
    SetRoute t (R FrontendRoute) m,
    Routed t (R FrontendRoute) m,
    PostBuild t m
  ) =>
  Bool ->
  m ()
page enableSearch = do
  Navbar.widget $
    Navbar.spacer >> Navbar.menu enableSearch
  Widget.card $ do
    elClass "div" "text-2xl font-bold" $
      text "Diverk"
    el "div" $ do
      el "div" $
        text "A minimalist GitHub repository browser"
      el "div" . Widget.link "https://github.com/jecaro/diverk" $
        text "Learn more"
