--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Char (toLower)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Hakyll
import Slug (toSlug)
import System.FilePath (takeBaseName, takeDirectory, (</>))
import Text.HTML.TagSoup (Tag (..))
import Text.Pandoc.Highlighting (Style, haddock, styleToCss)
import Text.Pandoc.Options (HighlightMethod (Skylighting), ReaderOptions (readerExtensions), WriterOptions (..), githubMarkdownExtensions)

--------------------------------------------------------------------------------

main :: IO ()
main = hakyllWith config $ do
    match ("images/*" .||. "fonts/Fira_Sans/*") $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    create ["css/syntax.css"] $ do
        route idRoute
        compile $ do
            makeItem $ styleToCss pandocCodeStyle

    match "personal/about.md" $ do
        route $ customRoute $ const "about/index.html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    projectTags <- buildTags "projects/*" (fromCapture "tags/*.html")
    postTags <- buildTags "posts/*" (fromCapture "tags/*.html")
    makeTagPage projectTags "Projects"
    makeTagPage postTags "Posts"

    match "posts/*" $ do
        let ctx = constField "type" "article" <> postCtxWithTags postTags
        route $ metadataRoute titleRoute
        compile $
            pandocCompiler'
                >>= loadAndApplyTemplate "templates/post.html" ctx
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexHtmls
                >>= cleanIndexUrls

    match "projects/*" $ do
        let ctx = constField "type" "article" <> postCtxWithTags projectTags
        route $ metadataRoute titleRoute
        compile $
            pandocCompiler'
                >>= loadAndApplyTemplate "templates/post.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexHtmls
                >>= cleanIndexUrls

    create ["posts.html"] $ do
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let postsCtx =
                    listField "posts" postCtx (return posts)
                        <> constField "title" "Posts"
                        <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" postsCtx
                >>= loadAndApplyTemplate "templates/default.html" postsCtx
                >>= relativizeUrls
                >>= cleanIndexHtmls
                >>= cleanIndexUrls

    create ["projects.html"] $ do
        route cleanRoute
        compile $ do
            projects <- recentFirst =<< loadAll "projects/*"
            let projectCtx =
                    listField "projects" postCtx (return projects)
                        <> constField "title" "Projects"
                        <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/projects.html" projectCtx
                >>= loadAndApplyTemplate "templates/default.html" projectCtx
                >>= relativizeUrls
                >>= cleanIndexHtmls
                >>= cleanIndexUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            projects <- recentFirst =<< loadAll "projects/*"

            let indexCtx =
                    listField "projects" postCtx (return projects)
                        <> listField "posts" postCtx (return posts)
                        <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/indexDefault.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexHtmls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            projects <- recentFirst =<< loadAll "projects/*"

            -- load individiual pages from a list (globs DO NOT work here)
            singlePages <- loadAll (fromList ["personal/about.md"])

            let pages = posts <> projects <> singlePages
                sitemapCtx =
                    constField "root" root
                        <> listField "pages" postCtx (return pages)

            makeItem ("" :: String)
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
                >>= cleanIndexHtmls
                >>= cleanIndexUrls

    create ["feed.rss"] $ do
        route idRoute
        compile (feedCompiler renderRss)

--------------------------------------------------------------------------------

readingTimeField :: String -> Context String
readingTimeField key =
    field key calculate
  where
    calculate :: Item String -> Compiler String
    calculate = pure . withTagList acc . itemBody
    acc ts = [TagText . show . max 1 $ time ts]
    -- M. Brysbaert, Journal of Memory and Language (2009) vol 109.
    -- DOI: 10.1016/j.jml.2019.104047
    time ts = foldr count 0 ts `div` 238
    count (TagText s) n = n + length (words s)
    count _ n = n

makeTagPage :: Tags -> String -> Rules ()
makeTagPage tags typ = tagsRules tags $ \tag pattern -> do
    let title = typ <> " tagged \"" <> tag <> "\""
    route cleanRoute
    compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx =
                constField "title" title
                    <> listField (toLower <$> typ) (postCtxWithTags tags) (return posts)
                    <> defaultContext

        makeItem ""
            >>= loadAndApplyTemplate "templates/tag.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls
            >>= cleanIndexHtmls
            >>= cleanIndexUrls

postCtx :: Context String
postCtx =
    constField "root" root
        <> dateField "date" "%B %e, %Y"
        <> readingTimeField "readingtime"
        <> defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

titleRoute :: Metadata -> Routes
titleRoute meta = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> fileNameFromTitle meta </> "index.html"
      where
        p = toFilePath ident

fileNameFromTitle :: Metadata -> FilePath
fileNameFromTitle = T.unpack . toSlug . T.pack . getTitleFromMeta

getTitleFromMeta :: Metadata -> String
getTitleFromMeta = fromMaybe "no title" . lookupString "title"

config :: Configuration
config =
    defaultConfiguration
        { destinationDirectory = "docs"
        , previewPort = 5000
        }

root :: String
root = "https://haukenes.me"

feedConfiguration :: FeedConfiguration
feedConfiguration =
    FeedConfiguration
        { feedTitle = "Haukenes.me"
        , feedDescription = "Posts about my interrests and other things I have worked on."
        , feedAuthorName = "Jonas Haukenes"
        , feedAuthorEmail = ""
        , feedRoot = "https://haukenes.me"
        }

feedCtx :: Context String
feedCtx = postCtx <> bodyField "Description"

type FeedRenderer =
    FeedConfiguration ->
    Context String ->
    [Item String] ->
    Compiler (Item String)

feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer =
    renderer feedConfiguration feedCtx
        =<< fmap (take 10) . recentFirst
        =<< loadAllSnapshots "posts/*" "content"

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
      where
        p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
  where
    pattern = "/index.html"
    replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise = url
  where
    idx = "index.html"

pandocCodeStyle :: Style
pandocCodeStyle = haddock

pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
    pandocCompilerWith
        defaultHakyllReaderOptions
            { readerExtensions = githubMarkdownExtensions
            }
        defaultHakyllWriterOptions
            { writerHighlightMethod = Skylighting pandocCodeStyle
            }
