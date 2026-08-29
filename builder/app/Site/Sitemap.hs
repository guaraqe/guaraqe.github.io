{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Sitemap (buildSitemap) where

import Data.Time
import Development.Shake
import Development.Shake.FilePath
import Site.Blog (Post)
import Site.Blog qualified as Blog
import Site.Config qualified as Config

buildSitemap :: FilePath -> [Post] -> [Post] -> Action ()
buildSitemap outputFolder posts texts = do
  now <- liftIO getCurrentTime
  let currentDate = formatTime defaultTimeLocale "%Y-%m-%d" now

      staticPages =
        [ (Config.baseUrl ++ "/", currentDate),
          (Config.baseUrl ++ "/posts", currentDate),
          (Config.baseUrl ++ "/texts", currentDate),
          (Config.baseUrl ++ "/cv.html", currentDate),
          (Config.baseUrl ++ "/projects.html", currentDate),
          (Config.baseUrl ++ "/courses", currentDate)
        ]

      postPages = map (entry "/posts") posts
      textPages = map (entry "/texts") texts

      allPages = staticPages ++ postPages ++ textPages

      sitemapXml =
        unlines $
          [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
            "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
          ]
            ++ map formatUrl allPages
            ++ [ "</urlset>"
               ]

  writeFile' (outputFolder </> "sitemap.xml") sitemapXml

-- | One entry for a listed page. Post URLs are normally relative to the
-- listing they appear in, but an entry can carry a root-relative URL instead —
-- the entropy note does, since it lives outside @\/posts@ — and that is used
-- as it stands.
entry :: String -> Post -> (String, String)
entry section post = (Config.baseUrl ++ path, post.sortDate)
  where
    path = case post.url of
      '/' : _ -> post.url
      relative -> section ++ "/" ++ relative

formatUrl :: (String, String) -> String
formatUrl (url, date) =
  "  <url>\n    <loc>" ++ url ++ "</loc>\n    <lastmod>" ++ date ++ "</lastmod>\n    <changefreq>monthly</changefreq>\n    <priority>0.8</priority>\n  </url>"
