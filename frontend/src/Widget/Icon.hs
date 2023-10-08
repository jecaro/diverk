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

import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core

solid :: Text
solid = "fa-solid"

icon :: Text -> DomBuilder t m => m ()
icon = flip iconClass mempty

iconClass :: DomBuilder t m => Text -> [Text] -> m ()
iconClass name classes =
  elClass
    "span"
    (T.unwords $ solid : name : classes)
    blank

house :: DomBuilder t m => m ()
house = icon houseName

houseName :: Text
houseName = "fa-house"

info :: DomBuilder t m => m ()
info = icon infoName

infoName :: Text
infoName = "fa-circle-info"

gear :: DomBuilder t m => m ()
gear = icon gearName

gearName :: Text
gearName = "fa-gear"

search :: DomBuilder t m => m ()
search = icon searchName

searchName :: Text
searchName = "fa-magnifying-glass"

eyeName :: Text
eyeName = "fa-eye"

eyeSlashName :: Text
eyeSlashName = "fa-eye-slash"

kebabName :: Text
kebabName = "fa-ellipsis-vertical"
