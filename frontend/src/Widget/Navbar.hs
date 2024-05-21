module Widget.Navbar (widget, menu, spacer) where

import Common.Route (FrontendRoute (..))
import Control.Monad (void)
import Data.GADT.Compare (geq)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Obelisk.Route (R, pattern (:/))
import Obelisk.Route.Frontend (Routed, SetRoute (setRoute), askRoute)
import Reflex.Dom.Core
import qualified Widget.Icon as Icon

widget :: (DomBuilder t m) => m () -> m ()
widget = elClass "div" "navbar sticky shadow top-0 flex px-4 gap-2 bg-base-200"

menu ::
  ( SetRoute t (R FrontendRoute) m,
    DomBuilder t m,
    Routed t (R FrontendRoute) m,
    PostBuild t m
  ) =>
  Bool ->
  m ()
menu enableSearch = do
  elClass "div" "dropdown dropdown-end" $ do
    elAttr "label" ("tabindex" =: "0" <> "class" =: "btn btn-ghost btn-circle") $
      elClass "div" (T.unwords [Icon.solid, Icon.kebabName]) blank
    elMenu
  where
    elMenu =
      elAttr
        "ul"
        ( "tabindex" =: "0" <> "class"
            =: T.unwords
              [ "mt-3",
                "p-2",
                "shadow",
                "menu",
                "menu-compact",
                "dropdown-content",
                "rounded-box",
                "bg-base-200"
              ]
        )
        $ do
          dyRoute <- askRoute
          let dyOnCurrent route = not . similar route <$> dyRoute

          elMenuItem Icon.house (MkBrowse :/ []) "Browse" dyOnCurrent
          -- Search should only be available if there is a token. That's a
          -- requirement of the GitHub API.
          elMenuItem Icon.search (MkSearch :/ []) "Search" $
            fmap (&& enableSearch) . dyOnCurrent
          elMenuItem Icon.gear (MkSettings :/ ()) "Settings" dyOnCurrent
          elMenuItem Icon.info (MkAbout :/ ()) "About" dyOnCurrent

    elMenuItem icon route label dyRouteEnable = do
      let dyRouteEnable' = dyRouteEnable route
      (e, _) <- elDynClass' "li" (liClass <$> dyRouteEnable') $
        elClass "div" "flex items-center gap-2" $ do
          void icon
          text label
      let evClickIfRouteEnable = gate (current dyRouteEnable') $ domEvent Click e
      setRoute $ route <$ evClickIfRouteEnable

    liClass True = mempty
    liClass False = "disabled"

    similar (x :/ _) (y :/ _) = isJust $ geq x y

spacer :: DomBuilder t m => m ()
spacer = elClass "div" "flex-1" blank
