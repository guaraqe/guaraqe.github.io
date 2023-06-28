{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Site.Book
  ( Section (..)
  , build,
  )
where

import Control.Monad (forM, forM_, void, when)
import Data.Aeson
import Data.List qualified as L
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Forward
import GHC.Generics (Generic)
import Site.Html
import Site.Json
import Site.Pandoc
import Slick
import Site.Layout qualified as Layout
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

build :: FilePath -> FilePath -> FilePath -> Action Section
build outputFolder inputFolder outputPath = do
  section <-
    buildSection
      SectionParams
        { outputFolder,
          inputFolder,
          outputPath,
          number = []
        }

  let indexHtml = buildSubsectionsIndexHtml section.subsections
      bookIndex = TL.unpack $ renderHtml indexHtml

  writeSection outputFolder (setBookInfo bookIndex section.title section)

  pure section

--------------------------------------------------------------------------------
-- Section

data Section = Section
  { title :: String,
    url :: String,
    content :: String,
    subsections :: [Section],
    parentTitle :: Maybe String,
    parentUrl :: Maybe String,
    previousTitle :: Maybe String,
    previousUrl :: Maybe String,
    nextTitle :: Maybe String,
    nextUrl :: Maybe String,
    bookTitle :: Maybe String,
    bookIndex :: Maybe String
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

setBookInfo :: String -> String -> Section -> Section
setBookInfo index title section =
  section
    { bookTitle = Just title
    , bookIndex = Just index
    , subsections = fmap (setBookInfo index title) section.subsections
    }

writeSection :: FilePath -> Section -> Action ()
writeSection outputFolder section = do
  let postPath = outputFolder </> drop 1 section.url
      value = toJSON section
  template <- compileTemplate' "site/templates/book-section.html"
  let layout = Layout.Layout
         { title = section.title
         , content = T.unpack $ substitute template value
         , latex = True
         , page = "Courses"
         , pageLink = "/courses"
         }
  Layout.build postPath layout
  forM_ section.subsections $ \subsection ->
    writeSection outputFolder subsection

data SectionParams = SectionParams
  { inputFolder :: FilePath,
    outputFolder :: FilePath,
    outputPath :: FilePath,
    number :: [Int]
  }

toContext :: [a] -> [(Maybe a, a, Maybe a)]
toContext xs =
  let
    ps = [Nothing] <> fmap Just xs
    ns = fmap Just (drop 1 xs) <> [Nothing]
  in
    zip3 ps xs ns

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

    let fullPostData =
          setObjectAttribute "url" postUrl
            . setObjectAttribute "subsections" subsections
            . setObjectAttribute "index" index
            . addTitleNumbers number
            $ postData

    section :: Section <- convert fullPostData

    -- Do subsections first
    let subsectionsData =
         flip fmap (toContext subsections) $ \(mprevious, current, mnext) ->
           current
                { parentTitle = Just section.title,
                  parentUrl = Just section.url,
                  previousTitle = fmap (.title) mprevious,
                  previousUrl = fmap (.url) mprevious,
                  nextTitle = fmap (.title) mnext,
                  nextUrl = fmap (.url) mnext
                }

    template <- compileTemplate' "site/templates/book-section-content.html"
    let content = substitute template fullPostData

    convert .
      setObjectAttribute "content" content .
      setObjectAttribute "subsections" subsectionsData $
        fullPostData

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
