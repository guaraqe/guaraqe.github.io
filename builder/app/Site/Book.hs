{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Site.Book
  ( buildBook,
  )
where

import Control.Monad (forM, void, when)
import Data.Aeson
import Data.String (fromString)
import Data.Text qualified as T
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Forward
import GHC.Generics (Generic)
import Site.Html
import Site.Json
import Site.Pandoc
import Slick
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

buildBook :: FilePath -> FilePath -> FilePath -> Action ()
buildBook outputFolder inputFolder outputPath = do
  _ <- buildSection outputFolder inputFolder outputPath
  pure ()

--------------------------------------------------------------------------------
-- Section

data Section = Section
  { title :: String,
    content :: String,
    url :: String,
    subsections :: [Section]
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

buildSection :: FilePath -> FilePath -> FilePath -> Action Section
buildSection outputFolder inputFolder outputPath = do
  -- Build subsections
  subFolders <- getDirectoryDirs inputFolder
  let subsectionFolders = filter (/= "data") subFolders
  subsections <- forM subsectionFolders $ \subsectionFolder ->
    buildSection
      outputFolder
      (inputFolder </> subsectionFolder)
      (outputPath </> subsectionFolder)

  let indexHtml = buildSubsectionsIndexHtml subsections
      index = renderHtml indexHtml

  cacheAction ("section" :: T.Text, inputFolder) $ do
    -- Copy data folder
    let dataFolder = inputFolder </> "data"
        dataOutput = outputFolder </> outputPath </> "data"
    when ("data" `elem` subFolders) $ do
      filepaths <- getDirectoryFiles dataFolder ["*"]
      void $ forP filepaths $ \filepath ->
        copyFileChanged (dataFolder </> filepath) (dataOutput </> filepath)

    -- Compile section text
    let indexPath = inputFolder </> "index.md"
    postContent <- T.pack <$> readFile' indexPath

    (pandoc, meta) <- readMarkdownAndMeta postContent
    postData <- writeHtmlAndMeta pandoc meta

    let postUrl = "/" </> outputPath </> "index.html"
        postPath = outputFolder </> outputPath </> "index.html"

    let fullPostData =
          setObjectAttribute "url" postUrl
            . setObjectAttribute "subsections" subsections
            . setObjectAttribute "index" index
            $ postData

    template <- compileTemplate' "site/templates/book-section.html"

    writeFile' postPath $ T.unpack $ substitute template fullPostData
    convert fullPostData

--------------------------------------------------------------------------------
-- Index

buildSubsectionsIndexHtml :: [Section] -> Html
buildSubsectionsIndexHtml sections =
  addClasses $
    H.ul $
      foldMap buildIndexHtml sections

buildIndexHtml :: Section -> Html
buildIndexHtml Section {..} =
  H.li $
    mconcat
      [ H.a ! A.href (fromString url) $ fromString title,
        H.ul $ foldMap buildIndexHtml subsections
      ]
