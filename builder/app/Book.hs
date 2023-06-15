{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog
  ( buildBlog
  ) where

import Control.Lens
import Data.Aeson as A
import Data.Aeson.Lens
import Data.Text qualified as T
import Data.Time
import Control.Monad (void)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Forward
import GHC.Generics (Generic)
import Slick
import Slick.Pandoc
import Text.Pandoc

buildBlog :: FilePath -> FilePath -> Action ()
buildBlog inputFolder outputFolder = do
  allPosts <- buildPosts inputFolder outputFolder
  buildIndex outputFolder allPosts
  copyStaticFiles inputFolder outputFolder

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
buildIndex outputFolder posts' = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo {posts = posts'}
      indexHTML = T.unpack $ substitute indexT (toJSON indexInfo)
  writeFile' (outputFolder </> "index.html") indexHTML

--------------------------------------------------------------------------------
-- Posts

data Post = Post
  { title :: String,
    content :: String,
    url :: String,
    date :: String
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

buildPosts :: FilePath -> FilePath -> Action [Post]
buildPosts inputFolder outputFolder = do
  pPaths <- getDirectoryPaths ["*.md"] [inputFolder]
  forP pPaths (buildPost outputFolder)

buildPost :: FilePath -> FilePath -> Action Post
buildPost outputFolder srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO $ print srcPath
  postContent <- T.pack <$> readFile' srcPath
  let readOpts =
        def
          { readerExtensions = pandocExtensions
          }
      writeOpts = def
  postData <- markdownToHTMLWithOpts readOpts writeOpts postContent

  let postUrl = takeFileName $ srcPath -<.> "html"
      date = prefixToDate postUrl

  let fullPostData =
        _Object . at "date" ?~ String (T.pack (showGregorian date)) $
          _Object . at "url" ?~ String (T.pack postUrl) $
            postData

  template <- compileTemplate' "site/templates/post.html"

  writeFile' (outputFolder </> postUrl) . T.unpack $ substitute template fullPostData
  convert fullPostData

prefixToDate :: String -> Day
prefixToDate name =
  let (y, rest_m) = splitAt 4 name
      (m, rest_d) = splitAt 2 $ drop 1 rest_m
      d = take 2 $ drop 1 rest_d
   in fromGregorian (read y) (read m) (read d)
