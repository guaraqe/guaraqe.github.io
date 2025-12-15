{-# LANGUAGE OverloadedStrings #-}

module Site.Note
  ( buildNote,
  )
where

import Control.Monad (void, when)
import Data.Aeson (object, toJSON)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Forward
import Site.Json
import Site.Layout (Layout (..))
import Site.Layout qualified as Layout
import Site.Pandoc
import Slick
import qualified System.Directory as Dir

buildNote ::
  FilePath ->
  FilePath ->
  FilePath ->
  Maybe FilePath ->
  Action ()
buildNote outputFolder inputFile outputRel mAssetsDir = do
  markdown <- T.pack <$> readFile' inputFile
  (pandoc, meta) <- readMarkdownAndMeta markdown
  noteData <- writeHtmlAndMeta pandoc meta

  let title = fromMaybe "Note" (getObjectAttribute "title" noteData :: Maybe String)
      content = fromMaybe "" (getObjectAttribute "content" noteData :: Maybe String)
      noteUrl = "/" </> outputRel

  defaultTemplate <- compileTemplate' "site/templates/default.html"
  let wrapped =
        substitute
          defaultTemplate
          (object [("title", toJSON title), ("content", toJSON content)])

      layout =
        Layout
          { title = title,
            content = T.unpack wrapped,
            latex = True,
            page = "Notes",
            pageLink = "/notes",
            description = title ++ " - Notes",
            currentUrl = noteUrl,
            isPost = False
          }

  Layout.build (outputFolder </> outputRel </> "index.html") layout

  copyAssets outputFolder outputRel mAssetsDir

copyAssets :: FilePath -> FilePath -> Maybe FilePath -> Action ()
copyAssets outputFolder outputRel mAssetsDir =
  case mAssetsDir of
    Nothing -> pure ()
    Just assetsDir -> do
      exists <- liftIO $ Dir.doesDirectoryExist assetsDir
      when exists $ do
        files <- getDirectoryFiles assetsDir ["//*"]
        void $
          forP files $ \filepath ->
            copyFileChanged (assetsDir </> filepath) (outputFolder </> outputRel </> "images" </> filepath)
