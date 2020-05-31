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

    match "index.md" $ markdown "templates/home.html" $ defaultContext <>
        listField "posts" (defaultContext <> teaserContext) recentPosts

    match "blog/*.md" $ markdown "templates/post.html" defaultContext
  where
    recentPosts = take 5 <$> (loadAll "blog/*" >>= recentFirst)

-- | Compiles a Markdown file to an HTML page using the template and context.
markdown :: Identifier -> Context String -> Rules ()
markdown template context = do
    route $ setExtension "html"
    compile $ pandocCompilerWith readerOptions writerOptions
        >>= saveSnapshot beforeTemplates
        >>= loadAndApplyTemplate template context
        >>= loadAndApplyTemplate "templates/page.html" defaultContext
        >>= relativizeUrls
  where
    readerOptions = defaultHakyllReaderOptions
    writerOptions = defaultHakyllWriterOptions { writerHTMLMathMethod = KaTeX defaultKaTeXURL }

-- | The snapshot for compiled content before templates are applied.
beforeTemplates :: Snapshot
beforeTemplates = "beforeTemplates"

-- | A context with a `teaser` field.
teaserContext :: Context String
teaserContext = teaserFieldWithSeparator "<!-- more -->" "teaser" beforeTemplates
