module Widget.Navbar (widget, menu, spacer) where

import Control.Monad (void)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (Home, Search)
import qualified Route
import qualified Widget.Icon as Icon

widget :: (DomBuilder t m) => m () -> m ()
widget =
  elAttr "div" $
    "class" =: "navbar sticky shadow top-0 flex px-4 gap-2 bg-base-200"
      <> "style" =: "padding-top: calc(env(safe-area-inset-top) + 0.5rem)"

menu ::
  (DomBuilder t m, PostBuild t m, Route.Set t m, Route.Ask t m) =>
  Bool ->
  m ()
menu enableSearch = do
  dyRoute <- Route.ask
  let dyOnCurrent route = not . sameRoute route <$> dyRoute
  elClass "div" "dropdown dropdown-end" $ do
    elAttr "label" ("tabindex" =: "0" <> "class" =: "btn btn-ghost btn-circle") $
      elClass "div" (T.unwords [Icon.solid, Icon.kebabName]) blank
    elAttr
      "ul"
      ( "tabindex" =: "0"
          <> "class"
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
        elMenuItem Icon.house (Route.Browse []) "Browse" dyOnCurrent
        -- Search should only be available if there is a token. That's a
        -- requirement of the GitHub API.
        elMenuItem Icon.search (Route.Search []) "Search" $
          fmap (&& enableSearch) . dyOnCurrent
        elMenuItem Icon.gear Route.Settings "Settings" dyOnCurrent
        elMenuItem Icon.info Route.About "About" dyOnCurrent
  where
    elMenuItem icon route label dyRouteEnable = do
      let dyRouteEnable' = dyRouteEnable route
      (e, _) <- elDynClass' "li" (liClass <$> dyRouteEnable') $
        elClass "div" "flex items-center gap-2" $ do
          void icon
          text label
      let evClickIfRouteEnable = gate (current dyRouteEnable') $ domEvent Click e
      Route.set $ Route.Push route <$ evClickIfRouteEnable

    liClass True = mempty
    liClass False = "disabled"

    -- Compare route constructors, ignoring payloads
    sameRoute Route.Home Route.Home = True
    sameRoute (Route.Browse _) (Route.Browse _) = True
    sameRoute Route.Settings Route.Settings = True
    sameRoute (Route.Search _) (Route.Search _) = True
    sameRoute Route.About Route.About = True
    sameRoute _ _ = False

spacer :: (DomBuilder t m) => m ()
spacer = elClass "div" "flex-1" blank
