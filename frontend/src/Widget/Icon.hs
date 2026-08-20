module Widget.Icon
  ( eyeName,
    eyeSlashName,
    gear,
    house,
    iconClass,
    info,
    infoName,
    kebabName,
    search,
    searchName,
    solid,
  )
where

import qualified Data.Text as Text
import Reflex.Dom.Core

solid :: Text.Text
solid = "fa-solid"

icon :: Text.Text -> (DomBuilder t m) => m ()
icon = flip iconClass mempty

iconClass :: (DomBuilder t m) => Text.Text -> [Text.Text] -> m ()
iconClass name classes =
  elClass
    "span"
    (Text.unwords $ solid : name : classes)
    blank

house :: (DomBuilder t m) => m ()
house = icon houseName

houseName :: Text.Text
houseName = "fa-house"

info :: (DomBuilder t m) => m ()
info = icon infoName

infoName :: Text.Text
infoName = "fa-circle-info"

gear :: (DomBuilder t m) => m ()
gear = icon gearName

gearName :: Text.Text
gearName = "fa-gear"

search :: (DomBuilder t m) => m ()
search = icon searchName

searchName :: Text.Text
searchName = "fa-magnifying-glass"

eyeName :: Text.Text
eyeName = "fa-eye"

eyeSlashName :: Text.Text
eyeSlashName = "fa-eye-slash"

kebabName :: Text.Text
kebabName = "fa-ellipsis-vertical"
