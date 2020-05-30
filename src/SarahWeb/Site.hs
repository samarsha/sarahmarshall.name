{-# LANGUAGE OverloadedStrings #-}

module SarahWeb.Site (site) where

import GHC.IO.Encoding
import Hakyll

-- | The site generator.
site :: IO ()
site = setLocaleEncoding utf8 >> hakyll rules

-- | The file rules.
rules :: Rules ()
rules = do
    match "templates/*" $ compile templateBodyCompiler
    match "index.md" $ markdown "templates/home.html"
    match "posts/*.md" $ markdown "templates/post.html"

-- | Compiles a Markdown file to HTML and applies the template.
markdown :: Identifier -> Rules ()
markdown template = do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate template defaultContext
        >>= relativizeUrls
