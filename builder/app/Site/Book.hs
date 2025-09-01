{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Site.Book
  ( Section (..),
    build,
    buildList,
  )
where

import Control.Monad (forM, forM_, void, when)
import Data.Aeson
import Data.List qualified as L
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Development.Shake
import Network.HTTP.Client.TLS (newTlsManager)
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Forward
import GHC.Generics (Generic)
import Site.Html
import Site.Json
import Site.Layout qualified as Layout
import Site.Pandoc
import Site.Translate (translate)
import Slick
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

build :: FilePath -> FilePath -> FilePath -> Maybe String -> Action Section
build outputFolder inputFolder outputPath possibleLanguage = do
  section' <-
    buildSection
      SectionParams
        { outputFolder,
          inputFolder,
          outputPath,
          number = []
        }

  section <- case possibleLanguage of
    Nothing -> pure section'
    Just outLang ->
      translateSection ".translation-cache" "en" outLang section'

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
    { bookTitle = Just title,
      bookIndex = Just index,
      subsections = fmap (setBookInfo index title) section.subsections
    }

writeSection :: FilePath -> Section -> Action ()
writeSection outputFolder section = do
  let postPath = outputFolder </> drop 1 section.url
      value = toJSON section
  template <- compileTemplate' "site/templates/book-section.html"
  let layout =
        Layout.Layout
          { title = section.title,
            content = T.unpack $ substitute template value,
            latex = True,
            page = "Courses",
            pageLink = "/courses",
            description = section.title ++ " - Course material by Juan Raphael Diaz Simões",
            currentUrl = section.url,
            isPost = False
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
  let ps = [Nothing] <> fmap Just xs
      ns = fmap Just (drop 1 xs) <> [Nothing]
   in zip3 ps xs ns

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

  cacheAction ("section" :: T.Text, inputFolder, outputPath) $ do
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

    convert
      . setObjectAttribute "content" content
      . setObjectAttribute "subsections" subsectionsData
      $ fullPostData

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

--------------------------------------------------------------------------------
-- List
--
data ListInfo = ListInfo
  { courses :: [Section]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

buildList :: FilePath -> [Section] -> Action ()
buildList outputFolder sections = do
  indexT <- compileTemplate' "site/templates/course-list.html"
  let indexInfo = ListInfo sections
      layout =
        Layout.Layout
          { title = "Courses",
            content = T.unpack $ substitute indexT (toJSON indexInfo),
            latex = False,
            page = "Courses",
            pageLink = "/courses",
            description = "University courses on statistics, bioinformatics, and physics by Juan Raphael Diaz Simões",
            currentUrl = "/courses",
            isPost = False
          }
  Layout.build (outputFolder </> "index.html") layout

--------------------------------------------------------------------------------
-- Translation

translateSection :: FilePath -> String -> String -> Section -> Action Section
translateSection cachePath srcLang outLang section = do
    manager <- newTlsManager
    let trans = translate cachePath manager srcLang outLang

    title <- trans section.title
    content <- trans section.content
    parentTitle <- mapM trans section.parentTitle
    previousTitle <- mapM trans section.previousTitle
    nextTitle <- mapM trans section.nextTitle
    bookTitle <- mapM trans section.bookTitle
    bookIndex <- mapM trans section.bookIndex
    subsections <-
      mapM
        (translateSection cachePath srcLang outLang)
        section.subsections

    pure section
      { title = title
      , content = content
      , subsections = subsections
      , parentTitle = parentTitle
      , previousTitle = previousTitle
      , nextTitle = nextTitle
      , bookTitle = bookTitle
      , bookIndex = bookIndex
      }
