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

{-# LANGUAGE DeriveDataTypeable #-}

module Web.Lunarsite.Pygments
       (toHtml)
       where

import Control.Exception (throwIO,Exception)
import Data.Typeable (Typeable)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)

data PygmentsError = PygmentsError Int String
                   deriving Typeable

instance Show PygmentsError where
  show (PygmentsError code stderr) =
    printf "Pygmentize failed with code %d: %s" code stderr

instance Exception PygmentsError

toHtml :: String -> String -> IO String
toHtml language code = do
  (exitStatus, stdout, stderr) <- readProcessWithExitCode "pygmentize" args code
  case exitStatus of
    ExitFailure exitCode -> throwIO (PygmentsError exitCode stderr)
    ExitSuccess -> return stdout
  where args = ["-f", "html", "-Oencoding=utf-8", "-P", "nowrap", "-l", language]
