{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map

import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime,formatTime)
import Hakyll
import System.Locale (iso8601DateFormat,defaultTimeLocale)
import Text.Printf (printf)

addDate :: Metadata -> Routes
addDate metadata = customRoute addDateToIdentifier
  where
    format = iso8601DateFormat Nothing
    date :: UTCTime
    date = fromJust $ do
      value <- Map.lookup "published" metadata
      parseTime defaultTimeLocale format value
    prefix = formatTime defaultTimeLocale "%Y/%m/%d/" date
    addDateToIdentifier identifier = prefix ++ toFilePath identifier

postRoute :: Routes
postRoute = gsubRoute "posts/" (const "") `composeRoutes`
            metadataRoute addDate `composeRoutes`
            setExtension "html"

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

tagsCtx :: Tags -> Context String
tagsCtx = tagsField "tags"

tagPageCtx :: String -> [Item String] -> Context String
tagPageCtx tag posts =
  constField "title" (printf "Posts tagged “%s”" tag) <>
  listField "posts" postCtx (return posts) <>
  defaultContext

main :: IO ()
main = hakyll $ do
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  tagsRules tags $ \tag pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let context = tagPageCtx tag posts
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" context
        >>= loadAndApplyTemplate "templates/page.html" context
        >>= loadAndApplyTemplate "templates/default.html" context
        >>= relativizeUrls

  create ["tags/index.html"] $ do
    route idRoute
    compile $ do
      cloud <- renderTagCloud 100 130 tags
      let context = constField "title" "Tags" <> defaultContext
      makeItem cloud
        >>= loadAndApplyTemplate "templates/page.html" context
        >>= loadAndApplyTemplate "templates/default.html" context
        >>= relativizeUrls

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "pages/*" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html" defaultContext
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "posts/*" $ do
    route postRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    (tagsCtx tags <> postCtx)
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  create ["archive.html"] $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let archiveCtx =
                  listField "posts" postCtx (return posts) <>
                  constField "title" "Archives"            <>
                  defaultContext
          makeItem ""
              >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
              >>= loadAndApplyTemplate "templates/default.html" archiveCtx
              >>= relativizeUrls

  match "index.html" $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let indexCtx =
                  listField "posts" postCtx (return posts) <>
                  constField "title" "Home"                <>
                  defaultContext
          getResourceBody
              >>= applyAsTemplate indexCtx
              >>= loadAndApplyTemplate "templates/default.html" indexCtx
              >>= relativizeUrls

  match "templates/*" $ compile templateCompiler
