module Widget (card, error, spinner, link) where

import Common.Route (FrontendRoute (..))
import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.Route (R)
import Obelisk.Route.Frontend
  ( RouteToUrl,
    Routed,
    SetRoute,
    askRoute,
    routeLinkDynAttr,
  )
import Reflex.Dom.Core hiding (link)
import qualified Widget.Icon as Icon
import Prelude hiding (error)

spinner :: DomBuilder t m => m ()
spinner =
  elClass
    "div"
    ( T.unwords
        [ "absolute",
          "right-1/2",
          "bottom-1/2",
          "transform",
          "translate-x-1/2",
          "translate-y-1/2"
        ]
    )
    $ elClass
      "div"
      ( T.unwords
          [ "border-t-transparent",
            "border-solid",
            "animate-spin",
            "rounded-full",
            "border-primary",
            "border-4",
            "h-8",
            "w-8"
          ]
      )
      blank

error ::
  ( RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    DomBuilder t m,
    Prerender t m,
    Routed t (R FrontendRoute) m,
    PostBuild t m
  ) =>
  Text ->
  m ()
error msg = do
  route <- askRoute
  elClass "div" "p-4" $
    elClass "div" (T.unwords ["alert", "alert-error", "shadow-lg"]) $
      do
        el "div" $ do
          Icon.iconClass Icon.infoName mempty
          el "div" $ do
            elClass "h3" "font-bold" $ text "An error occurred "
            elClass "div" "text-xs" $ text msg
        el "div" $
          routeLinkDynAttr (constDyn $ "class" =: "link") route $
            text "try again"

card :: DomBuilder t m => m a -> m a
card =
  elClass "div" "flex items-start md:h-full md:pt-[20vh]"
    . elClass
      "div"
      ( T.unwords
          [ "flex",
            "flex-col",
            "md:rounded-lg",
            "md:bg-base-200",
            "md:max-w-md",
            "md:shadow",
            "w-screen",
            "w-full",
            "mx-auto",
            "gap-4",
            "p-4"
          ]
      )

link :: DomBuilder t m => Text -> m () -> m ()
link url = elAttr "a" ("class" =: "link" <> "href" =: url)
