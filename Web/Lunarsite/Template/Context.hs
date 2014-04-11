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

module Web.Lunarsite.Template.Context
       (defaultContext,postContext,tagsContext,archivePageContext)
       where

import qualified Web.Lunarsite.Info as I

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Hakyll.Web.Template.Context as C

import Data.Monoid ((<>),mconcat)
import Hakyll.Core.Identifier (toFilePath)
import Hakyll.Core.Item (Item(itemIdentifier))
import Hakyll.Web.Html (toUrl)
import Hakyll.Web.Tags (Tags,tagsFieldWith,getTags)
import Hakyll.Web.Template.Context (Context)
import Text.Blaze.Html (toValue,toHtml,(!))

infoContext :: Context String
infoContext = C.constField "siteCopyright" I.siteCopyright <>
              C.constField "siteAuthor" I.siteAuthor <>
              C.constField "siteAuthorEmail" I.siteAuthorEmail <>
              C.constField "siteCopyright" I.siteCopyright <>
              C.constField "siteRepository" I.siteRepository

defaultContext :: Context String
defaultContext = infoContext <> C.defaultContext

sourceField :: String -> Context a
sourceField key = C.field key (return.I.itemSource.toFilePath.itemIdentifier)

postContext :: Context String
postContext = sourceField "source" <>
              C.dateField "date" "%B %e, %Y" <>
              defaultContext

tagsContext :: Tags -> Context String
tagsContext = tagsFieldWith getTags renderLink (mconcat.concatLinks) "tags"
  where
    concatLinks [] = []
    concatLinks (x:xs) = x : concatTail xs
    concatTail [] = []
    concatTail (x:[]) = [" and ", x]
    concatTail (x:xs) = ", " : x : concatTail xs
    renderLink _   Nothing         = Nothing
    renderLink tag (Just filePath) =
      Just $ H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag

archivePageContext :: String -> String -> [Item String] -> Context String
archivePageContext title feed posts =
  C.constField "title" title <>
  C.constField "feed" feed <>
  C.listField "posts" postContext (return posts) <>
  defaultContext
