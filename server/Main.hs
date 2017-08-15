{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Text.Lazy  (Text)
import           System.EasyFile ((</>))
import           Web.Scotty

main :: IO ()
main =
    scotty 8000 $ do

        -- Routes to the start page.
        get "/index.html" startPage
        get "/" $ redirect "/index.html"

        -- Serving local JavaScript.
        get "/js/:file" $
            serveFile "js" "application/javascript; charset=utf-8" =<< param "file"

        -- Serving local CSS.
        get "/css/:file" $ serveFile "css" "text/css; charset=utf-8" =<< param "file"

        -- Serving (PNG) image files.
        get "/image/:file" $ serveFile "image" "image/png" =<< param "file"

        -- Serving (PNG) texture files.
        get "/texture/:file" $ serveFile "texture" "image/png" =<< param "file"

        -- Serving (MP3) music files.
        get "/music/:file" $ serveFile "music" "audio/mpeg" =<< param "file"

startPage :: ActionM ()
startPage = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    file $ "site" </> "index.html"

serveFile :: FilePath -> Text -> FilePath -> ActionM ()
serveFile resDir contentType requestedFile = do
    setHeader "content-Type" contentType
    file $ "site" </> resDir </> requestedFile
