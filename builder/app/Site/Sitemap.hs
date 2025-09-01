{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Site.Sitemap (buildSitemap) where

import Data.Time
import Development.Shake
import Development.Shake.FilePath
import Site.Blog (Post)
import qualified Site.Blog as Blog

buildSitemap :: FilePath -> [Post] -> Action ()
buildSitemap outputFolder posts = do
  now <- liftIO getCurrentTime
  let currentDate = formatTime defaultTimeLocale "%Y-%m-%d" now
      
      staticPages = 
        [ ("https://guaraqe.github.io/", currentDate)
        , ("https://guaraqe.github.io/posts", currentDate)
        , ("https://guaraqe.github.io/cv.html", currentDate)
        , ("https://guaraqe.github.io/projects.html", currentDate)
        , ("https://guaraqe.github.io/courses", currentDate)
        ]
      
      postPages = map (\post -> 
        ("https://guaraqe.github.io/posts/" ++ post.url, 
         take 10 $ post.date)) posts
      
      allPages = staticPages ++ postPages
      
      sitemapXml = unlines $
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
        ] ++ map formatUrl allPages ++
        [ "</urlset>"
        ]
  
  writeFile' (outputFolder </> "sitemap.xml") sitemapXml

formatUrl :: (String, String) -> String
formatUrl (url, date) = 
  "  <url>\n    <loc>" ++ url ++ "</loc>\n    <lastmod>" ++ date ++ "</lastmod>\n    <changefreq>monthly</changefreq>\n    <priority>0.8</priority>\n  </url>"
