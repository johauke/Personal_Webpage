--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import qualified Data.Text as T
import Hakyll
import Slug (toSlug)

--------------------------------------------------------------------------------

main :: IO ()
main = hakyllWith config $ do
    match ("images/*") $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "posts/*" $ do
        let ctx = constField "type" "article" <> postCtx
        route $ metadataRoute titleRoute
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "projects/*" $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/projects.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts)
                        <> constField "title" "Archives"
                        <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["projects.html"] $ do
        route idRoute
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

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts)
                        <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            -- load and sort the post
            posts <- recentFirst =<< loadAll "posts/*"

            -- load individiual pages from a list (globs DO NOT work here)
            singlePages <- loadAll (fromList ["about.rst", "contact.markdown"])

            -- mapped the posts and singlePages together
            let pages = posts <> singlePages
                -- create the `pages` field with the postCtx
                -- and return the `pages` value for it
                sitemapCtx =
                    constField "root" root
                        <> listField "pages" postCtx (return pages)

            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    create ["rss.xml"] $ do
        route idRoute
        compile (feedCompiler renderRss)

--------------------------------------------------------------------------------

type FeedRenderer =
    FeedConfiguration ->
    Context String ->
    [Item String] ->
    Compiler (Item String)

postCtx :: Context String
postCtx =
    constField "root" root
        <> dateField "date" "%B %e, %Y"
        <> defaultContext

titleRoute :: Metadata -> Routes
titleRoute = constRoute . fileNameFromTitle

fileNameFromTitle :: Metadata -> FilePath
fileNameFromTitle = T.unpack . (`T.append` ".html") . toSlug . T.pack . getTitleFromMeta

getTitleFromMeta :: Metadata -> String
getTitleFromMeta = fromMaybe "no title" . lookupString "title"

config :: Configuration
config =
    defaultConfiguration
        { destinationDirectory = "docs"
        , previewPort = 5000
        }

root :: String
root = "https://mypage.com"

feedConfiguration :: FeedConfiguration
feedConfiguration =
    FeedConfiguration
        { feedTitle = "My Blog"
        , feedDescription = "Posts about x, y & z"
        , feedAuthorName = "Jonas"
        , feedAuthorEmail = ""
        , feedRoot = "https://example.com"
        }

feedCtx :: Context String
feedCtx = postCtx <> bodyField "Description"

feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer =
    renderer feedConfiguration feedCtx
        =<< fmap (take 10) . recentFirst
        =<< loadAllSnapshots "posts/*" "content"
