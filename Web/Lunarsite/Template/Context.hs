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
       (postContext,tagsContext,archivePageContext)
       where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>),mconcat)
import Hakyll.Core.Item (Item)
import Hakyll.Web.Html (toUrl)
import Hakyll.Web.Tags (Tags,tagsFieldWith,getTags)
import Hakyll.Web.Template.Context (Context
                                   ,defaultContext
                                   ,dateField,constField,listField)
import Text.Blaze.Html (toValue,toHtml,(!))

postContext :: Context String
postContext = dateField "date" "%B %e, %Y" <> defaultContext

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
  constField "title" title <>
  constField "feed" feed <>
  listField "posts" postContext (return posts) <>
  defaultContext
