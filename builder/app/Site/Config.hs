module Site.Config
  ( baseUrl,
    ogImage,
  )
where

-- | Canonical origin for the site. This must match @site/CNAME@, otherwise
-- canonical URLs, Open Graph URLs and the sitemap point search engines at a
-- host different from the one actually serving the pages.
baseUrl :: String
baseUrl = "https://guaraqe.com"

-- | Absolute URL of the image used in social link previews.
ogImage :: String
ogImage = baseUrl ++ "/images/og-card.png"
