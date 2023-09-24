{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Translate
  ( translate,
  )
where

import Data.Aeson qualified as JSON
import Data.Aeson ((.=), (.:))
import Data.Hashable
import Data.Text (Text)
import Data.Text qualified as Text
import Development.Shake
import Network.HTTP.Client

translate :: FilePath -> Manager -> String -> String -> String -> Action String
translate cachePath manager srcLang outLang text = do
  let folder = cachePath <> "/" <> srcLang <> "-" <> outLang
      file = show (hash text)
      path = folder <> "/" <> file

  doesFileExist path >>= \case
    False -> do
      translation' <- liftIO $
        googleTranslate manager srcLang outLang text

      let translation = Text.unpack $ Text.replace "< /" "</" translation'

      writeFile' path translation

      pure translation
    True ->
      readFile' path

apiKey :: String
apiKey = ""

googleTranslate :: Manager -> String -> String -> String -> IO Text
googleTranslate manager srcLang outLang text = do
  initReq <- parseRequest $ "POST https://translation.googleapis.com/language/translate/v2?key=" <> apiKey

  let req = initReq
        { requestHeaders =
            [  ("Content-Type", "application/json; charset=utf-8")
            ]
        , requestBody = RequestBodyLBS $ JSON.encode $ JSON.object
            [ "q" .= [ text ]
            , "source" .= srcLang
            , "target" .= outLang
            , "format" .= ("html" :: String)
            ]
        }

  body <- responseBody <$> httpLbs req manager

  case JSON.eitherDecode body of
    Left e -> error $ e <> show body
    Right (Translation result) -> pure result

newtype Translation = Translation Text

instance JSON.FromJSON Translation where
   parseJSON = JSON.withObject "data" $ \o1 -> do
     o2 <- o1 .: "data"
     o3:_ <- o2 .: "translations"
     Translation <$> (o3 .: "translatedText")
