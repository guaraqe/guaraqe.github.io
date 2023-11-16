{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Pandoc
  ( readMarkdownAndMeta,
    writeHtmlAndMeta,
  )
where

import Data.Aeson (Value)
import Data.Text
import Development.Shake
import Site.Html
import Site.Json
import Slick.Pandoc
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Pandoc
  ( Pandoc (..),
    ReaderOptions (..),
    WriterOptions (..),
    HTMLMathMethod (..),
    def,
    pandocExtensions,
    runIO,
  )
-- import Image.LaTeX.Render
-- import Image.LaTeX.Render.Pandoc
import Text.Pandoc.CrossRef
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml5)

readMarkdownAndMeta :: Text -> Action (Pandoc, Value)
readMarkdownAndMeta markdown = do
  let readOpts =
        def
          { readerExtensions = pandocExtensions
          }

      reader = readMarkdown readOpts

  makePandocReader reader markdown

writeHtmlAndMeta :: Pandoc -> Value -> Action Value
writeHtmlAndMeta pandoc value = do
  let writeOpts =
        def
          { writerNumberSections = True,
            writerReferenceLinks = True,
            writerHTMLMathMethod = KaTeX ""
          }

  pandocWithRefs <-
    liftIO $
      runCrossRefIO defaultMeta Nothing defaultCrossRefAction pandoc

  -- pandocWithFormulas <- liftIO $
  -- convertAllFormulaeDataURI defaultEnv defaultPandocFormulaOptions pandocWithRefs

  html <-
    liftIO (runIO (writeHtml5 writeOpts pandocWithRefs)) >>= \case
      Left e -> fail (show e)
      Right r -> pure r

  let htmlWithClasses = addClasses html
      htmlText = renderHtml htmlWithClasses

  pure $ setObjectAttribute "content" htmlText value
