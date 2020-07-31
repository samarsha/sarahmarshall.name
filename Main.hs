{-# LANGUAGE OverloadedStrings #-}

-- | The static site generator for www.sarahmarshall.name.
module Main (main) where

import GHC.IO.Encoding
import Hakyll
import Text.Pandoc.Highlighting
import Text.Pandoc.Options

-- | Runs the site generator.
main :: IO ()
main = setLocaleEncoding utf8 >> hakyll rules

-- | The file rules.
rules :: Rules ()
rules = do
    copy "assets/**"
    copy "superposition-wiqca/**"

    create ["assets/code.css"] $ do
        route idRoute
        compile $ makeItem $ styleToCss tango

    match "templates/*" $ compile templateBodyCompiler

    match "index.md" $ markdown "templates/home.html" $
        listField "posts" teaserContext recentPosts <> defaultContext

    match "blog/*.md" $ markdown "templates/post.html" postContext
  where
    recentPosts = take 5 <$> loadAll "blog/*" >>= recentFirst

copy :: Pattern -> Rules ()
copy pattern = match pattern $ do
    route idRoute
    compile copyFileCompiler

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

-- | The post context with a `teaser` field.
teaserContext :: Context String
teaserContext = teaserFieldWithSeparator "<!-- more -->" "teaser" beforeTemplates <> postContext

-- | The default context with a formatted `date` field.
postContext :: Context String
postContext = dateField "date" "%B %e, %Y" <> defaultContext
