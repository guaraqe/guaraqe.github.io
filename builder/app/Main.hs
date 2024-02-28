{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Control.Monad
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Forward
import Site.Blog qualified as Blog
import Site.Book qualified as Book
import Site.Layout qualified as Layout
import Site.Json
import Data.Text qualified as Text
import Site.Pandoc qualified as Pandoc
import Slick

outputFolder :: FilePath
outputFolder = "docs/"

copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "site" ["images//*", "css//*", "js//*"]
  void $ forP filepaths $ \filepath ->
    copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

copyBioinformaticsExam :: Action ()
copyBioinformaticsExam = do
  let path = "/home/juan/Code/guaraqe/tdsi-statistics-exam/build-github"
  filepaths <- getDirectoryFiles path ["//*"]
  void $ forP filepaths $ \filepath ->
    copyFileChanged (path </> filepath) (outputFolder </> "courses" </> "statistics-exam" </> filepath)

copyStatisticsExam :: Action ()
copyStatisticsExam = do
  let path = "/home/juan/Code/guaraqe/tdsi-questions/build-github"
  filepaths <- getDirectoryFiles path ["//*"]
  void $ forP filepaths $ \filepath ->
    copyFileChanged (path </> filepath) (outputFolder </> "courses" </> "bioinformatics-exam" </> filepath)

buildBlog :: Action [Blog.Post]
buildBlog = do
  posts <- Blog.build "site/posts" (outputFolder </> "posts")
  Blog.buildIndex (outputFolder </> "posts") posts
  pure posts

buildBooks :: Action ()
buildBooks = do
  let makeBook (input, output) =
        Book.build outputFolder input output Nothing

  sections <- mapM
    makeBook
    [ ( "/home/juan/Courses/Statistics",
        "courses/statistics"
      ),
      ( "/home/juan/Courses/Bioinformatics",
        "courses/bioinformatics"
      ),
      ( "/home/juan/Courses/Physics",
        "courses/physics"
      )
    ]

  Book.buildList (outputFolder </> "courses") sections

buildIndex :: [Blog.Post] -> Action ()
buildIndex posts = do
  let path :: FilePath = "site/index.md"
  postContent <- Text.pack <$> readFile' path
  (pandoc, meta) <- Pandoc.readMarkdownAndMeta postContent
  postData <- Pandoc.writeHtmlAndMeta pandoc meta

  let fullPostData =
          setObjectAttribute "posts" posts
          $ postData

  template <- compileTemplate' "site/templates/index.html"

  post <- convert fullPostData

  let layout =
        Layout.Layout
          { title = "Home",
            content = Text.unpack $ substitute template fullPostData,
            latex = True,
            page = "Home",
            pageLink = "/"
          }

  Layout.build (outputFolder </> "index.html") layout

  pure post

buildCV :: Action ()
buildCV = do
  let path :: FilePath = "site/cv.md"
  postContent <- Text.pack <$> readFile' path
  (pandoc, meta) <- Pandoc.readMarkdownAndMeta postContent
  postData <- Pandoc.writeHtmlAndMeta pandoc meta

  template <- compileTemplate' "site/templates/default.html"

  post <- convert postData

  let layout =
        Layout.Layout
          { title = "CV",
            content = Text.unpack $ substitute template postData,
            latex = True,
            page = "CV",
            pageLink = "/cv.html"
          }

  Layout.build (outputFolder </> "cv.html") layout

  pure post


buildProjects :: Action ()
buildProjects = do
  let path :: FilePath = "site/projects.md"
  postContent <- Text.pack <$> readFile' path
  (pandoc, meta) <- Pandoc.readMarkdownAndMeta postContent
  postData <- Pandoc.writeHtmlAndMeta pandoc meta

  template <- compileTemplate' "site/templates/default.html"

  post <- convert postData

  let layout =
        Layout.Layout
          { title = "Projects",
            content = Text.unpack $ substitute template postData,
            latex = True,
            page = "Projects",
            pageLink = "/projects.html"
          }

  Layout.build (outputFolder </> "projects.html") layout

  pure post


buildRules :: Action ()
buildRules = do
  posts <- buildBlog
  buildBooks
  buildIndex posts
  buildCV
  buildProjects
  copyStaticFiles
  copyBioinformaticsExam
  copyStatisticsExam

main :: IO ()
main = do
  let shOpts =
        forwardOptions $
          shakeOptions
            { shakeLintInside = ["site"]
            }
  shakeArgsForward shOpts buildRules
