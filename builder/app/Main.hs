{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Control.Monad
import Data.Aeson (object, toJSON)
import Data.Text qualified as Text
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Forward
import Site.Blog qualified as Blog
import Site.Book qualified as Book
import Site.Layout qualified as Layout
import Site.Note qualified as Note
import Site.Sitemap qualified as Sitemap
import Slick
import System.Directory qualified as Dir

outputFolder :: FilePath
outputFolder = "docs/"

copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "site" ["images//*", "css//*", "js//*", "data//*", "robots.txt", "CNAME"]
  void $ forP filepaths $ \filepath ->
    copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

copyBioinformaticsExam :: Action ()
copyBioinformaticsExam = do
  let path = "/home/juan/Code/guaraqe/tdsi-statistics-exam/build-github"
  copyOptionalDirectory path (outputFolder </> "courses" </> "statistics-exam")

copyStatisticsExam :: Action ()
copyStatisticsExam = do
  let path = "/home/juan/Code/guaraqe/tdsi-questions/build-github"
  copyOptionalDirectory path (outputFolder </> "courses" </> "bioinformatics-exam")

copyPhysicsExam :: Action ()
copyPhysicsExam = do
  let path = "/home/juan/Code/guaraqe/tdsi-physics-exam/build-github"
  copyOptionalDirectory path (outputFolder </> "courses" </> "physics-exam")

copyMicroMacro :: Action ()
copyMicroMacro = do
  let path = "/home/juan/Code/guaraqe/micro-macro/crates/micro-macro/web/dist-release"
  copyOptionalDirectory path (outputFolder </> "micro-macro")

copyPadreLevedo :: Action ()
copyPadreLevedo =
  copyOptionalDirectory "site/padre-levedo" (outputFolder </> "padre-levedo")

copyOptionalDirectory :: FilePath -> FilePath -> Action ()
copyOptionalDirectory input output = do
  exists <- liftIO $ Dir.doesDirectoryExist input
  when exists $ do
    filepaths <- getDirectoryFiles input ["//*"]
    void $ forP filepaths $ \filepath ->
      copyFileChanged (input </> filepath) (output </> filepath)

buildNotes :: Action ()
buildNotes = do
  let input = "/home/juan/Obsidian/Science/Generalized-Entropies/generalized-entropies.md"
      output = "notes/generalized-entropies"
      assets = Just "/home/juan/Obsidian/Science/Generalized-Entropies/images"
  exists <- liftIO $ Dir.doesFileExist input
  when exists $
    Note.buildNote outputFolder input output assets

buildBlog :: Action [Blog.Post]
buildBlog = do
  posts <- Blog.build "site/posts" (outputFolder </> "posts")
  Blog.buildIndex (outputFolder </> "posts") posts
  pure posts

buildTexts :: Action [Blog.Post]
buildTexts = do
  texts <- Blog.buildTexts "site/texts" (outputFolder </> "texts")
  Blog.buildTextsIndex (outputFolder </> "texts") texts
  pure texts

buildBooks :: Action ()
buildBooks = do
  let makeBook (input, output) =
        Book.build outputFolder input output Nothing

  let books =
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

  availableBooks <-
    filterM (liftIO . Dir.doesDirectoryExist . fst) books
  sections <- mapM makeBook availableBooks

  Book.buildList (outputFolder </> "courses") sections

buildIndex :: [Blog.Post] -> [Blog.Post] -> Action ()
buildIndex posts texts = do
  let path :: FilePath = "site/index.html"

  -- Compile the HTML content as a template and substitute posts data
  htmlTemplate <- compileTemplate' path
  let templateData =
        object
          [ ("posts", toJSON (take 5 posts)),
            ("texts", toJSON (take 3 texts))
          ]
      processedContent = substitute htmlTemplate templateData

  -- Apply the default template for proper layout
  defaultTemplate <- compileTemplate' "site/templates/default.html"
  let wrappedData = object [("title", toJSON ("" :: String)), ("content", toJSON (Text.unpack processedContent))]
      finalContent = substitute defaultTemplate wrappedData

  let layout =
        Layout.Layout
          { title = "Home",
            content = Text.unpack finalContent,
            language = "en",
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
            language = "en",
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
            language = "en",
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
  texts <- buildTexts
  buildBooks
  buildIndex posts texts
  buildCV
  buildProjects
  Sitemap.buildSitemap outputFolder posts texts
  copyStaticFiles
  copyBioinformaticsExam
  copyStatisticsExam
  copyPhysicsExam
  copyMicroMacro
  copyPadreLevedo
  buildNotes

main :: IO ()
main = do
  let shOpts =
        forwardOptions $
          shakeOptions
            { shakeLintInside = ["site"]
            }
  shakeArgsForward shOpts buildRules
