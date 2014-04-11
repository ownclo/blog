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

module Web.Lunarsite.Pandoc
       (transformingPandocCompiler)
       where

import qualified Web.Lunarsite.Pygments as P

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Monad (liftM)
import Network.URI (parseAbsoluteURI,URI(URI,uriScheme,uriPath))
import Hakyll.Core.Compiler (Compiler,getRoute,unsafeCompiler)
import Hakyll.Core.Identifier (fromFilePath)
import Hakyll.Core.Item (Item)
import Hakyll.Web.Pandoc (pandocCompilerWithTransformM
                         ,defaultHakyllReaderOptions
                         ,defaultHakyllWriterOptions)
import Text.Blaze.Html (preEscapedToHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 ((!))
import Text.Pandoc (Pandoc,Inline(Link),Block(CodeBlock,RawBlock),nullAttr)
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

pygmentizeCodeBlocks :: Block -> Compiler Block
pygmentizeCodeBlocks x@(CodeBlock attr _) | attr == nullAttr = return x
pygmentizeCodeBlocks x@(CodeBlock (_,[],_) _) = return x
pygmentizeCodeBlocks (CodeBlock (_,language:_,_) text) = do
  code <- liftM preEscapedToHtml (unsafeCompiler (P.toHtml language text))
  let colored = renderHtml (H.pre code ! A.class_ "highlight")
  return (RawBlock "html" colored)
pygmentizeCodeBlocks x = return x

transformDocument :: Pandoc -> Compiler Pandoc
transformDocument doc = walkM transformLinks doc >>= walkM pygmentizeCodeBlocks

transformingPandocCompiler :: Compiler (Item String)
transformingPandocCompiler =
  pandocCompilerWithTransformM defaultHakyllReaderOptions
  defaultHakyllWriterOptions transformDocument
