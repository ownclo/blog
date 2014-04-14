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

module Web.Lunarsite.Pandoc
       (transformingPandocCompiler)
       where

import Text.Highlighting.Pygments.Pandoc

import Network.URI (parseAbsoluteURI,URI(URI,uriScheme,uriPath))
import Hakyll.Core.Compiler (Compiler,getRoute,unsafeCompiler)
import Hakyll.Core.Identifier (fromFilePath)
import Hakyll.Core.Item (Item)
import Hakyll.Web.Pandoc (pandocCompilerWithTransformM
                         ,defaultHakyllReaderOptions
                         ,defaultHakyllWriterOptions)
import Text.Pandoc.Options (ReaderOptions,
                            WriterOptions(writerHtml5,writerHighlight))
import Text.Pandoc (Pandoc,Inline(Link))
import Text.Pandoc.Walk (walkM)
import Text.Printf (printf)


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
transformDocument doc =
  walkM transformLinks doc >>= unsafeCompiler.codeBlocksToHtml


readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions


writerOptions :: WriterOptions
writerOptions =
  -- We need HTML 5 output, since we are using bootstrap, and we disable the
  -- built-in highlighting of Pandoc, since we ship our own based on Pygments.
  defaultHakyllWriterOptions { writerHtml5 = True
                             , writerHighlight = False
                             }

transformingPandocCompiler :: Compiler (Item String)
transformingPandocCompiler =
  pandocCompilerWithTransformM readerOptions writerOptions transformDocument
