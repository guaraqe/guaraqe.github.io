{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}


module Site.Layout
  ( Layout (..),
    build,
  )
where

import Data.Aeson
import Data.Text qualified as T
import Development.Shake
import Development.Shake.Classes
import GHC.Generics (Generic)
import Slick

data Layout = Layout
  { title :: String,
    content :: String,
    latex :: Bool,
    page :: String,
    pageLink :: String,
    description :: String,
    currentUrl :: String,
    isPost :: Bool
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

build :: FilePath -> Layout -> Action ()
build outputPath layout = do
  let value = toJSON layout
  template <- compileTemplate' "site/templates/layout.html"
  writeFile' outputPath $ T.unpack $ substitute template value
