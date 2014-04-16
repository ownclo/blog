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
       (isAvailable,toHtml)
       where

import Foreign.Python

import qualified Data.ByteString.UTF8 as UTF8
import Control.Exception (tryJust)
import Control.Monad (liftM,guard)

type Lexer = PyObject
type Formatter = PyObject

-- |@'isAvailable'@ checkers whether Pygments is available or not.
--
-- If Pygments is not available, return @'Nothing'@.  Otherwise return @'Just'
-- version@, where @version@ is the version number of Pygments.
isAvailable :: IO (Maybe String)
isAvailable = do
  initialize False
  result <- tryJust (guard.isPythonException) (importModule "pygments")
  case result of
    Left _ -> return Nothing
    Right pygments -> do
      version <- getAttr pygments "__version__"
      liftM (Just . UTF8.toString) (fromPy version)

getLexerByName :: String -> IO Lexer
getLexerByName name = do
  initialize False
  lexers <- importModule "pygments.lexers"
  get_lexer_by_name <- getAttr lexers "get_lexer_by_name"
  pyName <- toPy name
  callObject get_lexer_by_name [pyName] []

highlight :: String -> Lexer -> Formatter -> IO String
highlight code lexer formatter = do
  initialize False
  pygments <- importModule "pygments"
  py_highlight <- getAttr pygments "highlight"
  codeObj <- toPy code
  callObject py_highlight [codeObj, lexer, formatter] [] >>= fromPy

toHtml :: String -> String -> IO String
toHtml code language = do
  lexer <- getLexerByName language
  formatters <- importModule "pygments.formatters"
  html_formatter <- getAttr formatters "HtmlFormatter"
  cssclass_key <- toPy "cssclass"
  cssclass <- toPy "highlight"
  formatter <- callObject html_formatter [] [(cssclass_key, cssclass)]
  highlight code lexer formatter
