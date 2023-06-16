module Site.Json
  ( setObjectAttribute,
  )
where

import Control.Lens (at, (?~))
import Data.Aeson
import Data.Aeson.Lens

setObjectAttribute :: ToJSON a => Key -> a -> Value -> Value
setObjectAttribute attr value =
  _Object . at attr ?~ toJSON value
