{-# LANGUAGE DeriveAnyClass #-}

module Site.Layout
  ( Layout (..)
  , build
  ) where

import Development.Shake
import Slick
import Development.Shake.Classes
import Data.Aeson
import GHC.Generics (Generic)
import Data.Text qualified as T

data Layout = Layout
  { title :: String
  , content :: String
  , latex :: Bool
  , page :: String
  , pageLink :: String
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

build :: FilePath -> Layout -> Action ()
build outputPath layout = do
  let value = toJSON layout
  template <- compileTemplate' "site/templates/layout.html"
  writeFile' outputPath $ T.unpack $ substitute template value
