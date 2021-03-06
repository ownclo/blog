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

module Web.Lunarsite.Main (main) where

import qualified Web.Lunarsite.Info as I
import qualified Text.Highlighting.Pygments as P
import Web.Lunarsite.Pandoc
import Web.Lunarsite.Routes
import Web.Lunarsite.Template.Context

import Data.Monoid ((<>))
import Hakyll hiding (defaultContext)
import System.Exit (exitFailure)
import System.FilePath (takeFileName)
import System.IO (stderr,hPutStrLn)
import Text.Printf (printf)

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle = "lunarsite — All posts"
  , feedDescription = I.siteDescription
  , feedAuthorName = I.siteAuthor
  , feedAuthorEmail = I.siteAuthorEmail
  , feedRoot = I.siteRoot
  }

hakyllConfiguration :: Configuration
hakyllConfiguration = defaultConfiguration {
    ignoreFile = includeSomeHiddenFiles (ignoreFile defaultConfiguration)
  , deployCommand = "git push"  -- Travis CI will do the rest
  , inMemoryCache = True }
  where includeSomeHiddenFiles _ fn | takeFileName fn ==  ".nojekyll" = False
        includeSomeHiddenFiles ignore fn = ignore fn

checkPygments :: IO ()
checkPygments = do
  result <- P.isAvailable
  case result of
    Just pygmentsVersion -> putStrLn ("Using Pygments " ++ pygmentsVersion)
    Nothing -> do
      hPutStrLn stderr "Pygments not available!"
      exitFailure

main :: IO ()
main = hakyllWith hakyllConfiguration $ do
  preprocess checkPygments
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  tagFeeds <- buildTags "posts/*" (fromCapture "tags/*.atom")

  tagsRules tags $ \tag pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let context = archivePageContext
                    (printf "Posts tagged “%s”" tag)
                    (printf "/tags/%s.atom" tag)
                    posts tags
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" context
        >>= loadAndApplyTemplate "templates/archive-page.html" context
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
      cloud <- renderTagCloud 100 200 tags
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
      >>= loadAndApplyTemplate "templates/post.html" (tagsContext tags <> postContext)
      >>= loadAndApplyTemplate "templates/default.html" postContext

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let context = archivePageContext "Archives" "/feed.atom" posts tags
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" context
        >>= loadAndApplyTemplate "templates/archive-page.html" context
        >>= loadAndApplyTemplate "templates/default.html" context

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let context = archivePageContext "Home" "/feed.atom" (take 10 posts) tags
      getResourceBody
        >>= applyAsTemplate context
        >>= loadAndApplyTemplate "templates/default.html" context

  match "404.md" $ do
    route $ setExtension "html"
    compile $ transformingPandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext

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
  match (fromList [".nojekyll", "CNAME"]) $ do
    route idRoute
    compile copyFileCompiler
