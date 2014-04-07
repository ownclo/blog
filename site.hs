-- Copyright (c) 2014 Sebastian Wiesner <lunaryorn@gmail.com>

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import qualified Data.Map as Map
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Maybe (fromJust)
import Data.Monoid ((<>),mconcat)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime,formatTime)
import Hakyll
import Network.URI (parseAbsoluteURI,URI(URI,uriScheme,uriPath))
import System.Locale (iso8601DateFormat,defaultTimeLocale)
import Text.Pandoc (Pandoc,Inline(Link))
import Text.Pandoc.Walk (walkM)
import Text.Printf (printf)
import Text.Blaze.Html (toValue,toHtml,(!))

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle = "lunarsite — All posts"
  , feedDescription = "Emacs. What else?"
  , feedAuthorName = "Sebastian Wiesner"
  , feedAuthorEmail = "lunaryorn@gmail.com"
  , feedRoot = "http://www.lunaryorn.com"
  }

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
tagsCtx = tagsFieldWith getTags renderLink (mconcat.concatLinks) "tags"
  where
    concatLinks [] = []
    concatLinks (x:xs) = x : concatTail xs
    concatTail [] = []
    concatTail (x:[]) = [" and ", x]
    concatTail (x:xs) = ", " : x : concatTail xs
    renderLink _   Nothing         = Nothing
    renderLink tag (Just filePath) =
      Just $ H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag

archivePageCtx :: String -> [Item String] -> Context String
archivePageCtx title posts =
  constField "title" title <>
  listField "posts" postCtx (return posts) <>
  defaultContext

referenceEmacsLispSymbol :: String -> String -> String
referenceEmacsLispSymbol =
  printf "http://bruce-connor.github.io/emacs-online-documentation/%s%%2F%s"

transformSpecialSchemes :: URI -> Compiler (Maybe String)
transformSpecialSchemes URI{uriScheme=scheme,uriPath=path} =
  case scheme of
    "internal:" -> do
      target <- getRoute (fromFilePath path)
      return (fmap ("/"++) target)
    "el-variable:" -> return (Just (referenceEmacsLispSymbol "Var" path))
    "el-function:" -> return (Just (referenceEmacsLispSymbol "Fun" path))
    "el-face:" -> return (Just (referenceEmacsLispSymbol "Face" path))
    _ -> return Nothing

transformLinks :: Inline -> Compiler Inline
transformLinks link@(Link ref (url,title)) =
  case parseAbsoluteURI url of
    Nothing -> return link
    Just uri -> do
      transformed <- transformSpecialSchemes uri
      case transformed of
        Nothing -> return link
        Just newUrl -> return (Link ref (newUrl,title))
transformLinks x = return x

transformDocument :: Pandoc -> Compiler Pandoc
transformDocument = walkM transformLinks

transformingPandocCompiler :: Compiler (Item String)
transformingPandocCompiler =
  pandocCompilerWithTransformM defaultHakyllReaderOptions
  defaultHakyllWriterOptions transformDocument

main :: IO ()
main = hakyll $ do
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  tagFeeds <- buildTags "posts/*" (fromCapture "tags/*.atom")

  tagsRules tags $ \tag pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let context = archivePageCtx (printf "Posts tagged “%s”" tag) posts
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" context
        >>= loadAndApplyTemplate "templates/page.html" context
        >>= loadAndApplyTemplate "templates/default.html" context
        >>= relativizeUrls

  tagsRules tagFeeds $ \tag pattern -> do
    route idRoute
    compile $ do
      let title = printf "lunarsite – Posts tagged %s" tag
      let config = feedConfiguration{feedTitle = title}
      loadAllSnapshots pattern "postContent"
        >>= recentFirst
        >>= renderAtom config (defaultContext <> bodyField "description")

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
    compile $ transformingPandocCompiler
      >>= loadAndApplyTemplate "templates/page.html" defaultContext
      >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "posts/*" $ do
    route postRoute
    compile $ transformingPandocCompiler
      >>= saveSnapshot "postContent"
      >>= loadAndApplyTemplate "templates/post.html"    (tagsCtx tags <> postCtx)
      >>= loadAndApplyTemplate "templates/default.html" postCtx

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let context = archivePageCtx "Archives" posts
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" context
        >>= loadAndApplyTemplate "templates/page.html" context
        >>= loadAndApplyTemplate "templates/default.html" context

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx = listField "posts" postCtx (return (take 5 posts)) <>
                     constField "title" "Home" <>
                     defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx

  -- For compatibility with the old Tinkerer-based site
  create ["rss.html"] $ do
    route idRoute
    compile $ loadAllSnapshots "posts/*" "postContent"
        >>= recentFirst
        >>= renderRss feedConfiguration (defaultContext <> bodyField "description")

  create ["feed.atom"] $ do
    route idRoute
    compile $ loadAllSnapshots "posts/*" "postContent"
        >>= recentFirst
        >>= renderAtom feedConfiguration (defaultContext <> bodyField "description")

  match "templates/*" $ compile templateCompiler

  -- Github pages stuff
  create [".nojekyll"] $ do
    route idRoute
    compile empty

  create ["CNAME"] $ do
    route idRoute
    compile domain

  where domain :: Compiler (Item String)
        domain = makeItem "www.lunaryorn.com\n"
        empty :: Compiler (Item String)
        empty = makeItem ""
