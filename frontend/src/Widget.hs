module Widget (card, error, link, spinner) where

import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (link)
import Route (AskRoute (..), RouteToUrl, SetRoute (..), routeLink)
import qualified Widget.Icon as Icon
import Prelude hiding (error)

spinner :: (DomBuilder t m) => m ()
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
  ( DomBuilder t m,
    PostBuild t m,
    SetRoute t m,
    RouteToUrl m,
    AskRoute t m
  ) =>
  Text -> m ()
error msg = do
  elClass "div" "p-4" $
    elClass "div" (T.unwords ["alert", "alert-error", "shadow-lg"]) $
      do
        el "div" $ do
          Icon.iconClass Icon.infoName mempty
          el "div" $ do
            elClass "h3" "font-bold" $ text "An error occurred "
            elClass "div" "text-xs" $ text msg
        el "div" $ do
          dyRoute <- askRoute
          dyn_ $ ffor dyRoute $ \route ->
            routeLink route $ text "try again"

card :: (DomBuilder t m) => m a -> m a
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

link :: (DomBuilder t m) => Text -> m () -> m ()
link url = elAttr "a" ("class" =: "link" <> "href" =: url)
