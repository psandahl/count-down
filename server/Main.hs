{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           System.EasyFile ((</>))
import           Web.Scotty

main :: IO ()
main =
    scotty 8000 $ do

        -- Routes to the start page.
        get "/index.html" startPage
        get "/" $ redirect "/index.html"

        -- Serving local JavaScript.
        get "/js/:file" $ scriptFile =<< param "file"

        -- Serving local CSS.
        get "/css/:file" $ cssFile =<< param "file"

        -- Serving (PNG) image files.
        get "/image/:file" $ imageFile =<< param "file"

        -- Serving (PNG) texture files.
        get "/texture/:file" $ textureFile =<< param "file"

startPage :: ActionM ()
startPage = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    file $ "site" </> "index.html"

scriptFile :: FilePath -> ActionM ()
scriptFile script = do
    setHeader "Content-Type" "application/javascript; charset=utf-8"
    file $ "site" </> "js" </> script

cssFile :: FilePath -> ActionM ()
cssFile css = do
    setHeader "Content-Type" "text/css; charset=utf-8"
    file $ "site" </> "css" </> css

imageFile :: FilePath -> ActionM ()
imageFile image = do
    setHeader "Content-Type" "image/png"
    file $ "site" </> "image" </> image

textureFile :: FilePath -> ActionM ()
textureFile texture = do
    setHeader "Content-Type" "image/png"
    file $ "site" </> "texture" </> texture
