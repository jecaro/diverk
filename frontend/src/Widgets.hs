module Widgets (spinner, errorWidget) where

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
import Reflex.Dom.Core

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
            "border-blue-400",
            "border-4",
            "h-8",
            "w-8"
          ]
      )
      blank

errorWidget ::
  ( RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    DomBuilder t m,
    Prerender t m,
    Routed t (R FrontendRoute) m,
    PostBuild t m
  ) =>
  Text ->
  m ()
errorWidget msg = do
  route <- askRoute
  elClass "div" "flex flex-col p-4" $
    elClass
      "div"
      ( T.unwords
          [ "p-4",
            "mx-auto",
            "text-red-800",
            "border",
            "border-red-300",
            "rounded-lg",
            "bg-red-50"
          ]
      )
      $ do
        elClass "div" "flex items-center" $ do
          elClass "i" "fa-solid fa-circle-info mr-2" blank
          elClass "h3" "text-lg font-medium" $
            text "An error occurred"
        elClass "div" "text-sm" $ do
          text msg
          el "br" blank
          text "You can "
          routeLinkDynAttr
            (constDyn $ "class" =: "text-blue-600 hover:underline")
            route
            $ text "try again"
