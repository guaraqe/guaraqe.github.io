{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE LambdaCase #-}

module Site.Layout
  ( Layout (..),
    build,
  )
where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Development.Shake
import Development.Shake.Classes
import GHC.Generics (Generic)
import Site.Config qualified as Config
import Slick

data Layout = Layout
  { title :: String,
    content :: String,
    language :: String,
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
  let value = withSiteConfig (toJSON layout)
  template <- compileTemplate' "site/templates/layout.html"
  writeFile' outputPath $ T.unpack $ substitute template value

-- | Make the site-wide constants available to every layout template, so that
-- the canonical origin lives in exactly one place.
withSiteConfig :: Value -> Value
withSiteConfig = \case
  Object o ->
    Object $
      KeyMap.insert "baseUrl" (toJSON Config.baseUrl) $
        KeyMap.insert "ogImage" (toJSON Config.ogImage) o
  value -> value
