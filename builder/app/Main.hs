{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Control.Monad
import Data.Aeson (Value (Null), object, toJSON)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text qualified as Text
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Forward
import Site.Blog qualified as Blog
import Site.Layout qualified as Layout
import Site.Raw qualified as Raw
import Site.Sitemap qualified as Sitemap
import Slick
import System.Directory qualified as Dir

outputFolder :: FilePath
outputFolder = "docs/"

-- | Compile the Tailwind stylesheet. Pages used to load Tailwind from
-- @cdn.tailwindcss.com@, which is the in-browser JIT compiler and is not meant
-- for production, plus the whole of daisyUI for eleven classes. Building here
-- ships one small stylesheet and removes both third-party hosts from the
-- critical path.
buildStyles :: Action ()
buildStyles = do
  -- Tailwind decides what to emit by scanning the markup, so every file it is
  -- configured to scan is an input to this step.
  sources <-
    getDirectoryFiles
      ""
      [ "site//*.html",
        "site//*.md",
        "tailwind/*",
        "site/raw//*.html"
      ]
  need sources
  command_ [] "tailwindcss" $
    [ "--config",
      "tailwind/tailwind.config.js",
      "--input",
      "tailwind/input.css",
      "--output",
      outputFolder </> "css" </> "tailwind.css",
      "--minify"
    ]

copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "site" ["images//*", "css//*", "js//*", "data//*", "robots.txt", "CNAME"]
  void $ forP filepaths $ \filepath ->
    copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

-- | Padre Levedo is a personal humour blog that happens to share a domain with
-- the CV. It stays live and reachable by direct link, but every page is marked
-- noindex so it does not surface in search results beside professional pages.
copyPadreLevedo :: Action ()
copyPadreLevedo = do
  let input = "site/padre-levedo"
      output = outputFolder </> "padre-levedo"
  exists <- liftIO $ Dir.doesDirectoryExist input
  when exists $ do
    filepaths <- getDirectoryFiles input ["//*"]
    void $ forP filepaths $ \filepath ->
      if takeExtension filepath == ".html"
        then do
          contents <- readFile' (input </> filepath)
          writeFile' (output </> filepath) $
            Text.unpack $
              insertNoindex (Text.pack contents)
        else copyFileChanged (input </> filepath) (output </> filepath)

-- | Add a robots meta tag to a document, unless it already declares one or has
-- no head to put it in.
insertNoindex :: Text.Text -> Text.Text
insertNoindex contents
  | "name=\"robots\"" `Text.isInfixOf` contents = contents
  | Text.null rest = contents
  | otherwise = before <> headTag <> noindexTag <> Text.drop (Text.length headTag) rest
  where
    headTag = "<head>"
    noindexTag = "\n<meta name=\"robots\" content=\"noindex, nofollow\">"
    (before, rest) = Text.breakOn headTag contents

-- | The PDF CV is authored in Typst outside this repository. It is copied into
-- the output so that @/juan-simoes-cv.pdf@ is a stable link that can be given
-- to recruiters and attached to applications.
copyCV :: Action ()
copyCV = do
  let path = "/home/juan/Obsidian/Knowledge/CV/pdf/juan-simoes-cv.pdf"
  copyOptionalFile path (outputFolder </> "juan-simoes-cv.pdf")

-- | Apps built in their own repositories, which are expected to be checked
-- out next to this one and already built for the path they are published
-- under. The build output is copied as is; the build fails when it is missing.
copyApps :: Action ()
copyApps = do
  copyApp "../micro-macro/crates/micro-macro/web/dist-release" "micro-macro"
  copyApp "../nixos-module-navigator/viewer/dist" "nixos-module-navigator"

copyApp :: FilePath -> FilePath -> Action ()
copyApp input output = do
  exists <- liftIO $ Dir.doesDirectoryExist input
  filepaths <- if exists then getDirectoryFiles input ["//*"] else pure []
  when (null filepaths) $
    fail $
      "Missing build output: " <> input
  void $ forP filepaths $ \filepath ->
    copyFileChanged (input </> filepath) (outputFolder </> output </> filepath)

copyOptionalFile :: FilePath -> FilePath -> Action ()
copyOptionalFile input output = do
  exists <- liftIO $ Dir.doesFileExist input
  if exists
    then copyFileChanged input output
    else putWarn $ "Missing optional file, skipping: " <> input

-- | The entropy note. It used to be a nav item and a home page section of its
-- own, both pointing straight at the page because there was only ever one
-- note; it reads as writing, so it is listed with the writing. There is no
-- Markdown source to collect it from — see "Site.Raw" — so the listing entry
-- is spelled out here. The date is when the note first went up on the site.
entropyNote :: Blog.Post
entropyNote =
  Blog.Post
    { title = "Generalized effective numbers and entropies",
      content = "",
      url = noteUrl,
      fullUrl = noteUrl,
      date = "December 16, 2025",
      sortDate = "2025-12-16",
      summary = Just "Working notes on generalized effective numbers, Hill numbers, and related entropies.",
      tags = Just "Information Theory, Mathematics",
      tagList = Just ["Information Theory", "Mathematics"]
    }

-- | Root-relative, unlike the posts, whose URLs are relative to the listing
-- they appear in. It resolves the same from @\/@ and from @\/posts@.
noteUrl :: String
noteUrl = "/notes/generalized-entropies/"

buildBlog :: Action [Blog.Post]
buildBlog = do
  posts <- Blog.build "site/posts" (outputFolder </> "posts")
  let listed = sortOn (Down . (.sortDate)) (entropyNote : posts)
  Blog.buildIndex (outputFolder </> "posts") listed
  pure listed

buildTexts :: Action [Blog.Post]
buildTexts = do
  texts <- Blog.buildTexts "site/texts" (outputFolder </> "texts")
  Blog.buildTextsIndex (outputFolder </> "texts") texts
  pure texts

-- | The listing at @\/courses@. The course books themselves are no longer
-- built from Markdown — see "Site.Raw" — so the three entries are written out
-- in the template rather than collected from the sections.
buildCourseList :: Action ()
buildCourseList = do
  template <- compileTemplate' "site/templates/course-list.html"
  let layout =
        Layout.Layout
          { title = "Courses",
            content = Text.unpack $ substitute template (object []),
            language = "en",
            latex = False,
            page = "Courses",
            pageLink = "/courses",
            description = "University courses on statistics, bioinformatics, and physics by Juan Raphael Diaz Simões",
            currentUrl = "/courses",
            isPost = False
          }
  Layout.build (outputFolder </> "courses" </> "index.html") layout

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
  let wrappedData = object [("title", Null), ("content", toJSON (Text.unpack processedContent))]
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
  -- The page supplies its own <h1>; a null title suppresses the one in the
  -- default template so the page does not open with two headings. It has to be
  -- null rather than "": Mustache treats an empty string as a present value, so
  -- the {{#title}} section still fired and left an empty <h1> taking up space.
  let wrappedData = object [("title", Null), ("content", toJSON (Text.unpack htmlContent))]
      finalContent = substitute defaultTemplate wrappedData

  let layout =
        Layout.Layout
          { title = "CV",
            content = Text.unpack finalContent,
            language = "en",
            latex = True,
            page = "CV",
            pageLink = "/cv.html",
            description = "CV of Juan Raphael Diaz Simões - Senior backend and platform engineer specializing in Kubernetes, distributed systems, Haskell, Rust, Go, and observability",
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
  let wrappedData = object [("title", Null), ("content", toJSON (Text.unpack processedContent))]
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
  buildCourseList
  buildIndex posts texts
  buildCV
  buildProjects
  Sitemap.buildSitemap outputFolder posts texts
  Raw.build "site/raw" outputFolder
  buildStyles
  copyStaticFiles
  copyPadreLevedo
  copyCV
  copyApps

main :: IO ()
main = do
  let shOpts =
        forwardOptions $
          shakeOptions
            { shakeLintInside = ["site"]
            }
  shakeArgsForward shOpts buildRules
