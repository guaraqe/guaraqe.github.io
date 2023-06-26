{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Forward
import Site.Blog
import Site.Book

outputFolder :: FilePath
outputFolder = "docs/"

-- Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "site" ["images//*", "css//*", "js//*"]
  void $ forP filepaths $ \filepath ->
    copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

-- Specific build rules for the Shake system defines workflow to build the
-- website
buildRules :: Action ()
buildRules = do
  buildBlog "site/posts" (outputFolder </> "posts")
  buildBook
    outputFolder
    "/home/juan/courses/tdsi/statistics-book"
    ("courses" </> "statistics")
  buildBook
    outputFolder
    "/home/juan/courses/tdsi/bioinformatics"
    ("courses" </> "bioinformatics")
  copyStaticFiles

-- Kick it all off
main :: IO ()
main = do
  let shOpts =
        forwardOptions $
          shakeOptions
            { shakeLintInside = ["site"]
            }
  shakeArgsForward shOpts buildRules
