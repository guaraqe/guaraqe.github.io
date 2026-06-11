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
    buildTexts,
    buildTextsIndex,
  )
where

import Control.Monad (void)
import Data.Aeson as A
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
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
  posts <- buildPosts inputFolder outputFolder "posts" "Posts" "/posts"
  pure $ sortOn (Down . (.sortDate)) posts

buildTexts :: FilePath -> FilePath -> Action [Post]
buildTexts inputFolder outputFolder = do
  copyStaticFiles inputFolder outputFolder
  posts <- buildPosts inputFolder outputFolder "texts" "Texts" "/texts"
  pure $ sortOn (Down . (.sortDate)) posts

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
            language = "en",
            latex = False,
            page = "Posts",
            pageLink = "/posts",
            description = "Technical writing on programming languages, software engineering, bioinformatics, and performance optimization by Juan Raphael Diaz Simões",
            currentUrl = "/posts",
            isPost = False
          }
  Layout.build (outputFolder </> "index.html") layout

buildTextsIndex :: FilePath -> [Post] -> Action ()
buildTextsIndex outputFolder posts = do
  indexT <- compileTemplate' "site/templates/text-list.html"
  let indexInfo = IndexInfo posts
      layout =
        Layout.Layout
          { title = "Texts",
            content = T.unpack $ substitute indexT (toJSON indexInfo),
            language = "en",
            latex = False,
            page = "Texts",
            pageLink = "/texts",
            description = "Essays, stories, fragments, and other writing by Juan Raphael Diaz Simões",
            currentUrl = "/texts",
            isPost = False
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
    sortDate :: String,
    summary :: Maybe String
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

buildPosts :: FilePath -> FilePath -> FilePath -> String -> String -> Action [Post]
buildPosts inputFolder outputFolder urlRoot page pageLink = do
  pPaths <- getDirectoryPaths ["*.md"] [inputFolder]
  forP pPaths (buildPost outputFolder urlRoot page pageLink)

buildPost :: FilePath -> FilePath -> String -> String -> FilePath -> Action Post
buildPost outputFolder urlRoot page pageLink srcPath = cacheAction ("post" :: T.Text, srcPath) $ do
  postContent <- T.pack <$> readFile' srcPath
  (pandoc, meta) <- readMarkdownAndMeta postContent
  postData <- writeHtmlAndMeta pandoc meta

  let postUrl = takeFileName $ srcPath -<.> "html"
      date = prefixToDate postUrl

  let fullPostData =
        setObjectAttribute "date" (formatTime defaultTimeLocale "%B %e, %Y" date)
          . setObjectAttribute "sortDate" (showGregorian date)
          . setObjectAttribute "dateISO" (showGregorian date)
          . setObjectAttribute "url" postUrl
          . setObjectAttribute "fullUrl" (urlRoot </> postUrl)
          $ postData

  template <- compileTemplate' "site/templates/post.html"

  post <- convert fullPostData
  let language =
        T.unpack . T.strip . T.pack $
          fromMaybe "en" (getObjectAttribute "language" postData)

  let layout =
        Layout.Layout
          { title = post.title,
            content = T.unpack $ substitute template fullPostData,
            language,
            latex = True,
            page,
            pageLink,
            description = fromMaybe (take 160 $ post.title ++ " - Writing by Juan Raphael Diaz Simões") post.summary,
            currentUrl = "/" ++ urlRoot ++ "/" ++ postUrl,
            isPost = True
          }

  Layout.build (outputFolder </> postUrl) layout

  pure post

prefixToDate :: String -> Day
prefixToDate name =
  let (y, rest_m) = splitAt 4 name
      (m, rest_d) = splitAt 2 $ drop 1 rest_m
      d = take 2 $ drop 1 rest_d
   in fromGregorian (read y) (read m) (read d)
