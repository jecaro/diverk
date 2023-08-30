{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: $(static "css/styles.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Welcome to Obelisk!"
      el "p" $ text $ T.pack commonStuff

      el "div" $ do
        let
          cfg = "common/example"
          path = "config/" <> cfg
        getConfig cfg >>= \case
          Nothing -> text $ "No config file found in " <> path
          Just bytes -> case T.decodeUtf8' bytes of
            Left ue -> text $ "Couldn't decode " <> path <> " : " <> T.pack (show ue)
            Right s -> text s
      return ()
  }
