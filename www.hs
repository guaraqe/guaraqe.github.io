{-# LANGUAGE OverloadedStrings #-}

import Hakyll

import Data.Default (def)

--------------------------------------------------------------------------------

config = def
  { providerDirectory = "src"
  , destinationDirectory = "docs"
  }

main :: IO ()
main =
  hakyllWith config $ do

    match ("img/*" .||. "txt/en/img/*" .||. "txt/pt/img/*" .||. "data/**") $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler


    match (fromList ["cv.md", "projects.md"]) $ do
      route $ setExtension "html"
      compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match ("txt/en/*" .||. "txt/pt/*") $ do
      route $ setExtension "html"
      compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts_en <- recentFirst =<< loadAll "txt/en/*"
        posts_pt <- recentFirst =<< loadAll "txt/pt/*"
        let archiveCtx =
              listField "posts-en" postCtx (return posts_en) <>
              listField "posts-pt" postCtx (return posts_pt) <>
              constField "title" "Archives" <>
              defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls


    match "index.html" $ do
      route idRoute
      compile $ do
        posts_en <- recentFirst =<< loadAll "txt/en/*"
        posts_pt <- recentFirst =<< loadAll "txt/pt/*"

        let indexCtx =
              listField "posts-en" postCtx (return posts_en) <>
              listField "posts-pt" postCtx (return posts_pt) <>
              constField "title" "Home" <>
              defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"
       <> defaultContext
