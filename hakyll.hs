{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

import Hakyll
import Text.Pandoc
import Data.Monoid

--------------------------------------------------------------------------------

main :: IO ()
main = hakyllWith cfg $ do
    static
    index
    archive
    posts
    templates

cfg :: Configuration
cfg = defaultConfiguration

--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------

static :: Rules ()
static = do
    match "images/*" $ route idRoute >> compile copyFileCompiler
    match "css/*"    $ route idRoute >> compile compressCssCompiler

index :: Rules ()
index = match "index.md" $ do
    route   $ setExtension "html"
    compile $ compiler
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

archive :: Rules ()
archive = create ["posts.html"] $ do
    route $ setExtension "html"
    compile $ do
        allPosts <- recentFirst =<< loadAll "posts/*"
        makeItem ""
            >>= loadAndApplyTemplate "templates/list.html" (archiveCtx allPosts)
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

posts :: Rules ()
posts = match "posts/*" $ do
    route $ setExtension "html"
    compile $ compiler
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

templates :: Rules ()
templates = match "templates/*" $ compile templateCompiler

compiler :: Compiler (Item String)
compiler = pandocCompilerWith defaultHakyllReaderOptions pandocOptions

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "" }

--------------------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------------------

indexCtx :: Context String
indexCtx = constField "title" "Home" <> defaultContext

archiveCtx :: [Item String] -> Context String
archiveCtx postsList
   = listField "posts" postCtx (return postsList)
  <> constField "title" "Archives"
  <> defaultContext

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> mathCtx <> defaultContext

mathCtx :: Context String
mathCtx = field "mathjax" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    return $ if lookupString "mathjax" metadata == Nothing then "" else script
    where
    script =
        "<script type=\"text/javascript\" src=\"\
        \http://cdn.mathjax.org/mathjax/latest/MathJax.js?\
        \config=TeX-AMS-MML_HTMLorMML\">\
        \</script>"

