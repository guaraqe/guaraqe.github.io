{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Forward
import Site.Blog qualified as Blog
import Site.Book qualified as Book

outputFolder :: FilePath
outputFolder = "docs/"

copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "site" ["images//*", "css//*", "js//*"]
  void $ forP filepaths $ \filepath ->
    copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

buildBlog :: Action [Blog.Post]
buildBlog = do
  Blog.build "site/posts" (outputFolder </> "posts")

buildBooks :: Action [Book.Section]
buildBooks = do
  let makeBook (input, output) =
        Book.build outputFolder input output

  mapM
    makeBook
    [ ( "/home/juan/courses/tdsi/statistics-book",
        "courses" </> "statistics"
      ),
      ( "/home/juan/courses/tdsi/bioinformatics",
        "courses" </> "bioinformatics"
      )
    ]

buildRules :: Action ()
buildRules = do
  posts <- buildBlog
  Blog.buildIndex (outputFolder </> "posts") posts
  books <- buildBooks
  copyStaticFiles

main :: IO ()
main = do
  let shOpts =
        forwardOptions $
          shakeOptions
            { shakeLintInside = ["site"]
            }
  shakeArgsForward shOpts buildRules
