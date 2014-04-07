{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map

import Data.Maybe (fromJust)
import Data.Monoid (mappend)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime,formatTime)
import Hakyll
import System.Locale (iso8601DateFormat,defaultTimeLocale)

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

main :: IO ()
main = hakyll $ do
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
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
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
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
