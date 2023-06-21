module Site.Json
  ( setObjectAttribute,
    getObjectAttribute,
    overObjectAttribute,
  )
where

import Control.Lens (at, preview, (?~))
import Data.Aeson
import Data.Aeson.Lens (_Object)

setObjectAttribute :: ToJSON a => Key -> a -> Value -> Value
setObjectAttribute attr value =
  _Object . at attr ?~ toJSON value

getObjectAttribute :: FromJSON a => Key -> Value -> Maybe a
getObjectAttribute attr value =
  let fromJSON' Nothing = Nothing
      fromJSON' (Just x) = case fromJSON x of
        Error _ -> Nothing
        Success a -> Just a
   in preview (_Object . at attr) value >>= fromJSON'

overObjectAttribute ::
  (FromJSON a, ToJSON a) => Key -> (a -> a) -> Value -> Value
overObjectAttribute attr f value =
  case getObjectAttribute attr value of
    Nothing -> value
    Just a -> setObjectAttribute attr (f a) value
