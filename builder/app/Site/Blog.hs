{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Site.Blog
  ( Post (..),
    build,
    buildIndex,
  )
where

import Control.Monad (void)
import Data.Aeson as A
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text qualified as T
import Data.Time
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Forward
import GHC.Generics (Generic)
import Site.Json
import Site.Layout qualified as Layout
import Site.Pandoc
import Slick

build :: FilePath -> FilePath -> Action [Post]
build inputFolder outputFolder = do
  copyStaticFiles inputFolder outputFolder
  posts <- buildPosts inputFolder outputFolder
  pure $ sortOn (Down . (.date)) posts

--------------------------------------------------------------------------------
-- Static

copyStaticFiles :: FilePath -> FilePath -> Action ()
copyStaticFiles inputFolder outputFolder = do
  filepaths <- getDirectoryFiles inputFolder ["img//*"]
  void $ forP filepaths $ \filepath ->
    copyFileChanged (inputFolder </> filepath) (outputFolder </> filepath)

--------------------------------------------------------------------------------
-- Index

data IndexInfo = IndexInfo
  { posts :: [Post]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

buildIndex :: FilePath -> [Post] -> Action ()
buildIndex outputFolder posts = do
  indexT <- compileTemplate' "site/templates/post-list.html"
  let indexInfo = IndexInfo posts
      layout =
        Layout.Layout
          { title = "Posts",
            content = T.unpack $ substitute indexT (toJSON indexInfo),
            latex = False,
            page = "Posts",
            pageLink = "/posts"
          }
  Layout.build (outputFolder </> "index.html") layout

--------------------------------------------------------------------------------
-- Posts

data Post = Post
  { title :: String,
    content :: String,
    url :: String,
    fullUrl :: String,
    date :: String,
    summary :: Maybe String
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

buildPosts :: FilePath -> FilePath -> Action [Post]
buildPosts inputFolder outputFolder = do
  pPaths <- getDirectoryPaths ["*.md"] [inputFolder]
  forP pPaths (buildPost outputFolder)

buildPost :: FilePath -> FilePath -> Action Post
buildPost outputFolder srcPath = cacheAction ("post" :: T.Text, srcPath) $ do
  postContent <- T.pack <$> readFile' srcPath
  (pandoc, meta) <- readMarkdownAndMeta postContent
  postData <- writeHtmlAndMeta pandoc meta

  let postUrl = takeFileName $ srcPath -<.> "html"
      date = prefixToDate postUrl

  let fullPostData =
        setObjectAttribute "date" (showGregorian date)
          . setObjectAttribute "url" postUrl
          . setObjectAttribute "fullUrl" ("posts" </> postUrl)
          $ postData

  template <- compileTemplate' "site/templates/post.html"

  post <- convert fullPostData

  let layout =
        Layout.Layout
          { title = post.title,
            content = T.unpack $ substitute template fullPostData,
            latex = True,
            page = "Posts",
            pageLink = "/posts"
          }

  Layout.build (outputFolder </> postUrl) layout

  pure post

prefixToDate :: String -> Day
prefixToDate name =
  let (y, rest_m) = splitAt 4 name
      (m, rest_d) = splitAt 2 $ drop 1 rest_m
      d = take 2 $ drop 1 rest_d
   in fromGregorian (read y) (read m) (read d)
