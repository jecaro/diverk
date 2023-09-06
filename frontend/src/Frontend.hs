{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend (frontend) where

import Common.Model (GitTree (..))
import Common.Route (FinalRoute (..), FrontendRoute (..))
import Home (home)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Generated.Static (static)
import Obelisk.Route (R)
import Obelisk.Route.Frontend (askRoute, subPairRoute_, subRoute_, withRoutedT)
import Reflex.Dom.Core
import Tree (tree)

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = do
        el "title" $ text "Diverk"
        elAttr
          "link"
          ( "href" =: $(static "css/styles.css")
              <> "type" =: "text/css"
              <> "rel" =: "stylesheet"
          )
          blank,
      _frontend_body = do
        elAttr "div" ("class" =: "mt-4 mb-4 mr-4 ml-4 space-y-4") $ do
          subRoute_ $ \case
            MkHome -> home
            MkRepo -> withRoutedT (fmap toGitTree) $
              subPairRoute_ $ \gitTree ->
                subRoute_ $ \case
                  MkTree -> dyn_ . fmap (tree gitTree) =<< askRoute
    }
  where
    toGitTree (reOwner, (reRepo, (reBranch, route))) = (MkGitTree {..}, route)
