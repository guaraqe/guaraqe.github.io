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
import Data.List qualified as L
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
  _ <-
    buildSection
      SectionParams
        { outputFolder,
          inputFolder,
          outputPath,
          number = []
        }
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

data SectionParams = SectionParams
  { inputFolder :: FilePath,
    outputFolder :: FilePath,
    outputPath :: FilePath,
    number :: [Int]
  }

buildSection :: SectionParams -> Action Section
buildSection SectionParams {..} = do
  -- Build subsections
  subFolders <- getDirectoryDirs inputFolder
  let subsectionFolders =
        zip [1 ..] $
          filter (/= "data") subFolders
  subsections <- forM subsectionFolders $ \(subsectionNumber, subsectionFolder) ->
    buildSection
      SectionParams
        { inputFolder = (inputFolder </> subsectionFolder),
          outputFolder,
          outputPath = (outputPath </> subsectionFolder),
          number = number <> [subsectionNumber]
        }

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
            . addTitleNumbers number
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

addTitleNumbers :: [Int] -> Value -> Value
addTitleNumbers numbers =
  let prefix = L.intercalate "." (fmap show numbers) <> " "
   in overObjectAttribute "title" (prefix <>)
