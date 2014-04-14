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

module Text.Highlighting.Pygments.Pandoc
       (codeBlocksToHtml)
       where

import Text.Highlighting.Pygments (toHtml)

import Text.Pandoc (Format(..),Pandoc,Block(CodeBlock,RawBlock),nullAttr)
import Text.Pandoc.Walk (walkM)

-- |@'blockToHtml block'@ transforms a single code block to HTML.
--
-- A code block which has a language class is turned into a raw HTML block
-- containing the code highlighted by Pygments.
blockToHtml :: Block -> IO Block
blockToHtml x@(CodeBlock attr _) | attr == nullAttr = return x
blockToHtml x@(CodeBlock (_,[],_) _) = return x
blockToHtml (CodeBlock (_,language:_,_) text) = do
  colored <- toHtml text language
  return (RawBlock (Format "html") colored)
blockToHtml x = return x


-- |@'codeBlocksToHtml' document@ transforms all code blocks in the given
-- @document@ to raw HTML blocks containing the code highlighted by
-- pygments.
codeBlocksToHtml :: Pandoc -> IO Pandoc
codeBlocksToHtml = walkM blockToHtml
