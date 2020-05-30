{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import GHC.IO.Encoding
import Hakyll
import Text.Pandoc.Options

-- | The site generator.
main :: IO ()
main = setLocaleEncoding utf8 >> hakyll rules

-- | The file rules.
rules :: Rules ()
rules = do
    match "assets/**" $ do
        route idRoute
        compile copyFileCompiler
    match "templates/*" $ compile templateBodyCompiler
    match "index.md" $ markdown "templates/home.html"
    match "blog/*.md" $ markdown "templates/post.html"

-- | Compiles a Markdown file to an HTML page using the template.
markdown :: Identifier -> Rules ()
markdown template = do
    route $ setExtension "html"
    compile $ pandocCompilerWith
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions { writerHTMLMathMethod = KaTeX defaultKaTeXURL }
        >>= loadAndApplyTemplate template defaultContext
        >>= loadAndApplyTemplate "templates/page.html" defaultContext
        >>= relativizeUrls
