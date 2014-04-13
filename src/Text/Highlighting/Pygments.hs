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

module Text.Highlighting.Pygments
       (toHtml)
       where

import qualified Foreign.Python as Python

toHtml :: String -> String -> IO String
toHtml code language = do
  Python.initialize False
  pygments <- Python.importModule "pygments"
  formatters <- Python.importModule "pygments.formatters"
  lexers <- Python.importModule "pygments.lexers"
  html_formatter <- Python.getAttr formatters "HtmlFormatter"
  cssclass_key <- Python.toPy "cssclass"
  cssclass <- Python.toPy "highlight"
  formatter <- Python.callObject html_formatter [] [(cssclass_key, cssclass)]
  get_lexer_by_name <- Python.getAttr lexers "get_lexer_by_name"
  languageObj <- Python.toPy language
  lexer <- Python.callObject get_lexer_by_name [languageObj] []
  highlight <- Python.getAttr pygments "highlight"
  codeObj <- Python.toPy code
  highlighted_code <- Python.callObject highlight [codeObj, lexer, formatter] []
  Python.fromPy highlighted_code
