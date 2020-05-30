{-# LANGUAGE OverloadedStrings #-}

module SarahWeb.Site (site) where

import GHC.IO.Encoding
import Hakyll hiding (relativizeUrls)
import System.FilePath

import qualified System.FilePath.Posix as Posix

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
    compile $ pandocCompiler >>= loadAndApplyTemplate template defaultContext >>= relativizeUrls

-- | Compiles an item by converting absolute URLs to relative URLs. Fixes a bug with Hakyll's
-- `relativizeUrls` on Windows.
relativizeUrls :: Item String -> Compiler (Item String)
relativizeUrls item = do
    route <- getRoute $ itemIdentifier item
    return $ case route of
        Nothing -> item
        Just r -> (relativizeUrlsWith $ toSiteRoot $ pathToUrl r) <$> item

-- | Converts a relative `FilePath` to a relative URL.
pathToUrl :: FilePath -> String
pathToUrl = Posix.joinPath . splitDirectories
