{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Pandoc
  ( readMarkdownAndMeta,
    writeHtmlAndMeta,
  )
where

import Data.Aeson (Value)
import Data.Char (toLower)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
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
    Block (..),
    Inline (..),
    def,
    pandocExtensions,
    runIO,
  )
-- import Image.LaTeX.Render
-- import Image.LaTeX.Render.Pandoc
import Text.Pandoc.CrossRef
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml5)
import Text.Pandoc.Walk (walk)

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
            writerHTMLMathMethod = KaTeX "https://cdn.jsdelivr.net/npm/katex@0.16.8/dist"
          }

      pandocWithCallouts = convertCallouts pandoc

  pandocWithRefs <-
    liftIO $
      runCrossRefIO defaultMeta Nothing defaultCrossRefAction pandocWithCallouts

  -- pandocWithFormulas <- liftIO $
  -- convertAllFormulaeDataURI defaultEnv defaultPandocFormulaOptions pandocWithRefs

  html <-
    liftIO (runIO (writeHtml5 writeOpts pandocWithRefs)) >>= \case
      Left e -> fail (Prelude.show e)
      Right r -> pure r

  let htmlWithClasses = addClasses html
      htmlText = renderHtml htmlWithClasses

  pure $ setObjectAttribute "content" htmlText value

--------------------------------------------------------------------------------
-- Obsidian callouts

data CalloutKind
  = CalloutDefinition
  | CalloutTheorem

convertCallouts :: Pandoc -> Pandoc
convertCallouts = walk convertBlockQuote

convertBlockQuote :: Block -> Block
convertBlockQuote (BlockQuote (firstBlock : rest)) =
  case parseCalloutHeader firstBlock of
    Nothing -> BlockQuote (firstBlock : rest)
    Just info ->
      let titleBlock = buildTitle info
          attrId = T.pack $ fromMaybe "" (anchor info)
          attr =
            ( attrId,
              ["callout", calloutClass (kind info)],
              []
            )
          bodyBlocks =
            case body info of
              Nothing -> []
              Just inlines -> [Para inlines]
       in Div attr (titleBlock : bodyBlocks <> rest)
convertBlockQuote block = block

data CalloutInfo = CalloutInfo
  { kind :: CalloutKind,
    title :: [Inline],
    anchor :: Maybe String,
    body :: Maybe [Inline]
  }

parseCalloutHeader :: Block -> Maybe CalloutInfo
parseCalloutHeader = \case
  Para inlines -> parseInlines inlines
  Plain inlines -> parseInlines inlines
  _ -> Nothing
  where
    parseInlines (Str tag : rest)
      | Just calloutKind <- parseTag (T.unpack tag) =
          let trimmedRest = trimSpaces rest
              (headerRaw, bodyRest) = splitHeaderBody trimmedRest
              (titleInlines, anchorId) = stripAnchor headerRaw
              bodyInlines = nonEmptyList (trimSpaces bodyRest)
           in Just
                CalloutInfo
                  { kind = calloutKind,
                    title = titleInlines,
                    anchor = anchorId,
                    body = bodyInlines
                  }
    parseInlines _ = Nothing

calloutClass :: CalloutKind -> Text
calloutClass = \case
  CalloutDefinition -> "callout-definition"
  CalloutTheorem -> "callout-theorem"

calloutLabel :: CalloutKind -> Text
calloutLabel = \case
  CalloutDefinition -> "Definition"
  CalloutTheorem -> "Theorem"

parseTag :: String -> Maybe CalloutKind
parseTag rawTag =
  let lowerTag = fmap toLower rawTag
   in if "[!" `isPrefixOf` lowerTag && "]" `isSuffixOf` lowerTag
        then case Prelude.init (Prelude.drop 2 lowerTag) of
          "definition" -> Just CalloutDefinition
          "theorem" -> Just CalloutTheorem
          _ -> Nothing
        else Nothing

stripAnchor :: [Inline] -> ([Inline], Maybe String)
stripAnchor inlines =
  case reverse (trimSpaces inlines) of
    (Str anchor : rest) | "^" `T.isPrefixOf` anchor ->
      (trimSpaces (reverse rest), Just (T.unpack (T.drop 1 anchor)))
    _ -> (trimSpaces inlines, Nothing)

trimSpaces :: [Inline] -> [Inline]
trimSpaces = dropWhileEnd isSpaceLike . Prelude.dropWhile isSpaceLike

isSpaceLike :: Inline -> Bool
isSpaceLike = \case
  Space -> True
  SoftBreak -> True
  LineBreak -> True
  _ -> False

isSoftBreak :: Inline -> Bool
isSoftBreak = \case
  SoftBreak -> True
  LineBreak -> True
  _ -> False

splitHeaderBody :: [Inline] -> ([Inline], [Inline])
splitHeaderBody inlines =
  let (headerPart, rest) = break isSoftBreak inlines
      bodyPart = dropWhile isSoftBreak rest
   in (headerPart, bodyPart)

nonEmptyList :: [a] -> Maybe [a]
nonEmptyList [] = Nothing
nonEmptyList xs = Just xs

buildTitle :: CalloutInfo -> Block
buildTitle info =
  let label = calloutLabel (kind info)
      headerInlines =
        case title info of
          [] -> [Strong [Str label]]
          _ -> [Strong [Str (label <> ":")], Space] <> title info
   in Div ("", ["callout-title"], []) [Plain headerInlines]
