module Widget.Navbar (widget, liMenu, liSpacer) where

import Common.Route (FrontendRoute (..))
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Data.GADT.Compare (geq)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Obelisk.Route (R, pattern (:/))
import Obelisk.Route.Frontend (Routed, SetRoute (setRoute), askRoute)
import Reflex.Dom.Core
import Reflex.Extra (getGlobalClick)
import qualified Widget.Icon as Icon

widget ::
  ( DomBuilder t m
  ) =>
  m () ->
  m ()
widget =
  elClass "nav" "sticky shadow top-0 flex flex-col p-4 bg-white"
    . elClass "ol" "flex gap-x-4  w-full"

liMenu ::
  ( SetRoute t (R FrontendRoute) m,
    DomBuilder t m,
    MonadHold t m,
    MonadFix m,
    PostBuild t m,
    Prerender t m,
    Routed t (R FrontendRoute) m
  ) =>
  Bool ->
  m ()
liMenu enableSearch = elClass "li" "" $ do
  dyMenuVisible <- elMenuButton
  dyn_ . ffor dyMenuVisible $ \case
    True -> elMenu
    False -> blank
  where
    elMenuButton = do
      (e, _) <-
        elClass'
          "span"
          (T.unwords $ Icon.solid : Icon.kebabName : ["cursor-pointer", "px-2"])
          blank
      let evClickOnButton = domEvent Click e
      dyMouseOverButton <-
        toggle False $ leftmost [domEvent Mouseleave e, domEvent Mouseover e]
      evMouseClickElsewhere <-
        gate (not <$> current dyMouseOverButton) <$> getGlobalClick
      holdDyn False $
        leftmost [True <$ evClickOnButton, False <$ evMouseClickElsewhere]

    elMenu =
      elClass "div" "absolute top-[3.75rem] right-0 bg-white shadow" $ do
        elClass "ul" "flex flex-col" $ do
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
      (e, _) <- elDynClass' "li" (liClasses <$> dyRouteEnable') $
        elClass "div" "flex items-center gap-2" $ do
          void icon
          text label
      let evClickIfRouteEnable = gate (current dyRouteEnable') $ domEvent Click e
      setRoute $ route <$ evClickIfRouteEnable

    liClasses True = "px-4 py-2 hover:bg-gray-100 cursor-pointer"
    liClasses False = "px-4 py-2 hover:bg-gray-100 opacity-50"

    similar (x :/ _) (y :/ _) = isJust $ geq x y

liSpacer :: DomBuilder t m => m ()
liSpacer = elClass "li" "grow" blank
