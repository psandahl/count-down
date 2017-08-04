{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Web.Scotty

main :: IO ()
main =
    scotty 8000 $ do
        get "/index.html" startPage
        get "/" $ redirect "/index.html"

startPage :: ActionM ()
startPage = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    file "site/index.html"
