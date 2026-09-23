{-# LANGUAGE OverloadedStrings #-}

-- | Pages whose Markdown sources are gone.
--
-- The three course books and the entropy note were generated from directories
-- that lived outside this repository, on a machine that was stolen. What
-- survives is the built HTML. Freezing it in the output folder was the wrong
-- place for it: @make clean@ would destroy it, and a change to the header or
-- the stylesheet could never reach it. So the body of each page now lives
-- under @site\/raw@ as the source of record, and the layout is reapplied on
-- every build.
--
-- A file here is templated when it opens with a @---@ front matter block, and
-- copied byte for byte otherwise. That second path carries the images,
-- spreadsheets and handouts, and also the exam bundles, which are
-- self-contained pages that never used the site layout.
module Site.Raw
  ( build,
  )
where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Development.Shake
import Development.Shake.FilePath
import Site.Layout (Layout (..))
import Site.Layout qualified as Layout

build :: FilePath -> FilePath -> Action ()
build inputFolder outputFolder = do
  filepaths <- getDirectoryFiles inputFolder ["//*"]
  void $ forP filepaths $ \filepath -> do
    let input = inputFolder </> filepath
        output = outputFolder </> filepath
    if takeExtension filepath == ".html"
      then do
        contents <- T.pack <$> readFile' input
        case splitFrontMatter contents of
          Nothing -> copyFileChanged input output
          Just (frontMatter, content) ->
            Layout.build output (toLayout frontMatter content)
      else copyFileChanged input output

-- | Split @---@-delimited @key: value@ lines off the front of a document.
-- Nothing when the document does not open with such a block, which is how a
-- page opts out of the layout.
splitFrontMatter :: Text -> Maybe ([(Text, Text)], Text)
splitFrontMatter contents = do
  afterOpening <- T.stripPrefix opening contents
  let (block, rest) = T.breakOn closing afterOpening
  body <- T.stripPrefix closing rest
  pure (map parseLine (T.lines block), body)
  where
    opening = "---\n"
    closing = "\n---\n"
    parseLine line =
      let (key, value) = T.breakOn ":" line
       in (T.strip key, T.strip (T.drop 1 value))

toLayout :: [(Text, Text)] -> Text -> Layout
toLayout frontMatter content =
  Layout
    { title = get "title",
      content = T.unpack content,
      language = fromMaybe "en" (lookupText "language"),
      latex = lookup "latex" frontMatter == Just "true",
      page = get "page",
      pageLink = get "pageLink",
      description = get "description",
      currentUrl = get "url",
      -- Nothing here is a blog post: these are course sections and notes, and
      -- the flag only decides the schema.org type and the og:type.
      isPost = False
    }
  where
    lookupText key = T.unpack <$> lookup key frontMatter
    get key = fromMaybe "" (lookupText key)
