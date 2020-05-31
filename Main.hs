{-# LANGUAGE OverloadedStrings #-}

-- | The static site generator for www.sarahmarshall.name.
module Main (main) where

import GHC.IO.Encoding
import Hakyll
import Text.Pandoc.Options

-- | Runs the site generator.
main :: IO ()
main = setLocaleEncoding utf8 >> hakyll rules

-- | The file rules.
rules :: Rules ()
rules = do
    match "assets/**" $ do
        route idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateBodyCompiler

    match "index.md" $ markdown "templates/home.html" $ do
        posts <- take 5 <$> loadAll "blog/*" >>= recentFirst
        return $ defaultContext <>
            listField "posts" defaultContext (return posts)

    match "blog/*.md" $ markdown "templates/post.html" $ return defaultContext

-- | Compiles a Markdown file to an HTML page using the template and context.
markdown :: Identifier -> Compiler (Context String) -> Rules ()
markdown template context = do
    route $ setExtension "html"
    compile $ do
        ctx <- context
        pandocCompilerWith readerOptions writerOptions
            >>= loadAndApplyTemplate template ctx
            >>= loadAndApplyTemplate "templates/page.html" defaultContext
            >>= relativizeUrls
  where
    readerOptions = defaultHakyllReaderOptions
    writerOptions = defaultHakyllWriterOptions { writerHTMLMathMethod = KaTeX defaultKaTeXURL }
