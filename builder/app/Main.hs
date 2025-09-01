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
import Site.Sitemap qualified as Sitemap
import Data.Text qualified as Text
import Slick
import Data.Aeson (toJSON, object)

outputFolder :: FilePath
outputFolder = "docs/"

copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "site" ["images//*", "css//*", "js//*", "robots.txt"]
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

copyPhysicsExam :: Action ()
copyPhysicsExam = do
  let path = "/home/juan/Code/guaraqe/tdsi-physics-exam/build-github"
  filepaths <- getDirectoryFiles path ["//*"]
  void $ forP filepaths $ \filepath ->
    copyFileChanged (path </> filepath) (outputFolder </> "courses" </> "physics-exam" </> filepath)

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
  let path :: FilePath = "site/index.html"
  
  -- Compile the HTML content as a template and substitute posts data
  htmlTemplate <- compileTemplate' path
  let templateData = object [("posts", toJSON posts)]
      processedContent = substitute htmlTemplate templateData

  -- Apply the default template for proper layout
  defaultTemplate <- compileTemplate' "site/templates/default.html"
  let wrappedData = object [("title", toJSON ("Home" :: String)), ("content", toJSON (Text.unpack processedContent))]
      finalContent = substitute defaultTemplate wrappedData

  let layout =
        Layout.Layout
          { title = "Home",
            content = Text.unpack finalContent,
            latex = True,
            page = "Home",
            pageLink = "/",
            description = "Juan Raphael Diaz Simões - Software engineer specializing in Haskell, functional programming, bioinformatics, and performance optimization",
            currentUrl = "/",
            isPost = False
          }

  Layout.build (outputFolder </> "index.html") layout

buildCV :: Action ()
buildCV = do
  let path :: FilePath = "site/cv.html"
  htmlContent <- Text.pack <$> readFile' path

  -- Apply the default template for proper layout
  defaultTemplate <- compileTemplate' "site/templates/default.html"
  let wrappedData = object [("title", toJSON ("CV" :: String)), ("content", toJSON (Text.unpack htmlContent))]
      finalContent = substitute defaultTemplate wrappedData

  let layout =
        Layout.Layout
          { title = "CV",
            content = Text.unpack finalContent,
            latex = True,
            page = "CV",
            pageLink = "/cv.html",
            description = "CV of Juan Raphael Diaz Simões - Software engineer with expertise in functional programming, bioinformatics, and distributed systems",
            currentUrl = "/cv.html",
            isPost = False
          }

  Layout.build (outputFolder </> "cv.html") layout


buildProjects :: Action ()
buildProjects = do
  let path :: FilePath = "site/projects.html"
  htmlTemplate <- compileTemplate' path
  let templateData = object []
      processedContent = substitute htmlTemplate templateData
  defaultTemplate <- compileTemplate' "site/templates/default.html"
  let wrappedData = object [("title", toJSON ("Projects" :: String)), ("content", toJSON (Text.unpack processedContent))]
      finalContent = substitute defaultTemplate wrappedData

  let layout =
        Layout.Layout
          { title = "Projects",
            content = Text.unpack finalContent,
            latex = True,
            page = "Projects",
            pageLink = "/projects.html",
            description = "Open source projects and software by Juan Raphael Diaz Simões",
            currentUrl = "/projects.html",
            isPost = False
          }

  Layout.build (outputFolder </> "projects.html") layout


buildRules :: Action ()
buildRules = do
  posts <- buildBlog
  buildBooks
  buildIndex posts
  buildCV
  buildProjects
  Sitemap.buildSitemap outputFolder posts
  copyStaticFiles
  copyBioinformaticsExam
  copyStatisticsExam
  copyPhysicsExam

main :: IO ()
main = do
  let shOpts =
        forwardOptions $
          shakeOptions
            { shakeLintInside = ["site"]
            }
  shakeArgsForward shOpts buildRules
