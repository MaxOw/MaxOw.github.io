{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

import Control.Monad (liftM, void)
import Hakyll
import Text.Pandoc
import Data.Monoid
import System.FilePath (takeDirectory, replaceExtension)
import qualified System.Process as Process

--------------------------------------------------------------------------------

main :: IO ()
main = hakyllWith cfg $ do
    static
    templates
    index
    archive
    posts
    hireme
    cv_html; cv_pdf

cfg :: Configuration
cfg = defaultConfiguration

--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------

static :: Rules ()
static = do
    match "images/*" $ route idRoute >> compile copyFileCompiler
    match "css/*"    $ route idRoute >> compile compressCssCompiler
    match "bib/*"    $ compile biblioCompiler
    match "csl/*"    $ compile cslCompiler

index :: Rules ()
index = match "index.md" $ do
    route   $ setExtension "html"
    compile $ compiler
        >>= loadAndApplyTemplate "templates/nav.html"   postCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

archive :: Rules ()
archive = create ["posts.html"] $ do
    route $ setExtension "html"
    compile $ do
      allPosts <- recentFirst =<< loadAll "posts/*.md"
      makeItem ""
        >>= loadAndApplyTemplate "templates/list.html"   (archiveCtx allPosts)
        >>= loadAndApplyTemplate "templates/nav.html"     postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

posts :: Rules ()
posts = match "posts/*.md" $ do
    route $ setExtension "html"
    compile $ bibtexCompiler
        >>= loadAndApplyTemplate "templates/nav.html"       postCtx
        >>= loadAndApplyTemplate "templates/default.html"   postCtx
        >>= relativizeUrls

hireme :: Rules ()
hireme = match "other/hireme.md" $ do
    route $ constRoute "hireme.html"
    compile $ compiler
        >>= loadAndApplyTemplate "templates/nav.html"       postCtx
        >>= loadAndApplyTemplate "templates/default.html"   postCtx
        >>= relativizeUrls

cv_html :: Rules ()
cv_html = match "other/cv.md" $ do
    route $ constRoute "cv.html"
    compile $ compiler
        >>= loadAndApplyTemplate "templates/cv.html"        postCtx
        >>= loadAndApplyTemplate "templates/default.html"   postCtx
        >>= relativizeUrls

-- Following was taken from jaspervdj.be blog source
cv_pdf :: Rules ()
cv_pdf = match "other/cv.md" $ version "pdf" $ do
    route $ constRoute "cv.pdf"
    compile $ getResourceBody
        >>= readPandoc
        >>= (return . fmap writeXeTex)
        >>= loadAndApplyTemplate "templates/cv.tex" defaultContext
        >>= xelatex
    where
    writeXeTex = writeLaTeX def { writerTeXLigatures = False }

xelatex :: Item String -> Compiler (Item TmpFile)
xelatex item = do
    TmpFile texPath <- newTmpFile "xelatex.tex"
    let tmpDir  = takeDirectory texPath
    let pdfPath = replaceExtension texPath "pdf"

    unsafeCompiler $ do
        writeFile texPath $ itemBody item
        -- writeFile pdfPath ""
        void $ Process.system $ unwords
            [ "xelatex", "-halt-on-error"
            , "-output-directory" , tmpDir, texPath
            , ">/dev/null", "2>&1"]

    makeItem $ TmpFile pdfPath


templates :: Rules ()
templates = match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
-- Compilers
--------------------------------------------------------------------------------

bibtexCompiler :: Compiler (Item String)
bibtexCompiler = do
    csl <- load $ fromFilePath "csl/acm-siggraph.csl"
    -- csl <- load $ fromFilePath "csl/ieee-trigraph.csl"
    bib <- load $ fromFilePath "bib/refs.bib"
    liftM (writePandocWith pandocOptions) $
        getResourceBody >>= readPandocBiblio def csl bib

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

