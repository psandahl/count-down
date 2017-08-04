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

        -- Serving (PNG) texture files.
        get "/texture/:file" $ textureFile =<< param "file"

startPage :: ActionM ()
startPage = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    file $ "site" </> "index.html"

scriptFile :: FilePath -> ActionM ()
scriptFile script = do
    setHeader "Content-Type" "application/javascript"
    file $ "site" </> "js" </> script

textureFile :: FilePath -> ActionM ()
textureFile texture = do
    setHeader "Content-Type" "image/png"
    file $ "site" </> "texture" </> texture
