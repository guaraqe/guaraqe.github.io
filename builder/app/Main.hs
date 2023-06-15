{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Forward
import Blog

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
  copyStaticFiles

-- Kick it all off
main :: IO ()
main = do
  let shOpts =
        forwardOptions $
          shakeOptions
            { shakeVerbosity = Chatty,
              shakeLintInside = ["site"]
            }
  shakeArgsForward shOpts buildRules
