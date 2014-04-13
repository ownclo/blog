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

module Web.Lunarsite.Routes
       (addDate
       ,postRoute)
       where

import qualified Data.Map as Map

import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime,formatTime)
import Hakyll.Core.Identifier (toFilePath)
import Hakyll.Core.Metadata (Metadata)
import Hakyll.Core.Routes (Routes
                          ,composeRoutes
                          ,customRoute,gsubRoute,metadataRoute,setExtension)
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
