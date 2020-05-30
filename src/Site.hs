{-# LANGUAGE OverloadedStrings #-}

module Site
    ( site
    ) where

import GHC.IO.Encoding
import Hakyll

-- | The site generator.
site :: IO ()
site = setLocaleEncoding utf8 >> hakyll rules

-- | The file routing rules.
rules :: Rules ()
rules =
    match "*.md" $ do
        route $ setExtension "html"
        compile pandocCompiler
