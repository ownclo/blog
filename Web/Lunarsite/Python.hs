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
{-# LANGUAGE FlexibleInstances #-}

module Web.Lunarsite.Python
       (
         -- * Initialization
         initialize
         -- * Errors
       , PythonException(..)
         -- * Modules
       , importModule
         -- * Object access
       , getAttr
       , callObject
         -- * Value conversion
       , Object(..)
       )
       where

import Web.Lunarsite.Python.Native

import qualified Data.ByteString.UTF8 as UTF8
import Control.Exception (Exception,throwIO)
import Control.Monad (when,unless,liftM)
import Data.ByteString (useAsCStringLen,packCStringLen)
import Data.Typeable (Typeable)
import Foreign.C (withCAString)
import Foreign.Ptr (nullPtr)
import Foreign.ForeignPtr (newForeignPtr,withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

data PythonException = PythonException deriving (Typeable,Show)

instance Exception PythonException

-- |@'toPyObject' object@ converts the raw @object@ into a managed pointer.
--
-- A managed point will automatically de-reference the object pointed to when it
-- goes out of scope.
toPyObject :: RawPyObject -> IO PyObject
toPyObject = newForeignPtr pyDecRef

-- |Like 'toPyObject', but checks for Python exceptions when the object is
-- 'nullPtr'.
toPyObjectChecked :: RawPyObject -> IO PyObject
toPyObjectChecked obj = do
  when (obj == nullPtr) throwCurrentPythonException
  toPyObject obj

-- |Throw an exception representing the current Python exception.
throwCurrentPythonException :: IO ()
throwCurrentPythonException = do
  errorOccurred <- pyErr_Occurred
  unless (errorOccurred == nullPtr) (pyErr_PrintEx 0)
  throwIO PythonException

-- |@'initialize' signalHandlers@ initializes the interpreter.
--
-- When @signalHandlers@ is true, install Python's signal handlers.
initialize :: Bool -> IO ()
initialize True = pyInitializeEx 1
initialize False = pyInitializeEx 0

-- |@'importModule' name@ imports the Python module with the given 'name'.
--
-- Throw a 'PythonException' if the import failed.
importModule :: String -> IO PyObject
importModule modName =
  withCAString modName pyImport_ImportModule >>= toPyObjectChecked

-- |@'getAttr' object attribute@ gets the value of @attribute@ from @object@.
--
-- Throw a 'PythonException' if the attribute access failed.
getAttr :: PyObject -> String -> IO PyObject
getAttr obj attr = withForeignPtr obj $ \raw ->
  withCAString attr (pyObject_GetAttrString raw) >>= toPyObjectChecked

-- |@'callObject' object args kwargs@ calls a callable @object@ with the given
-- @args@ and @kwargs@.
--
-- Throw a 'PythonException' if the call failed.
callObject :: PyObject -> [PyObject] -> [(PyObject, PyObject)] -> IO PyObject
callObject obj args kwargs = do
  argsObj <- asTuple args
  kwargsObj <- asDict kwargs
  withForeignPtr argsObj $ \rawArgsObj ->
    withForeignPtr kwargsObj $ \rawKwargsObj ->
    withForeignPtr obj $ \raw ->
    pyObject_Call raw rawArgsObj rawKwargsObj >>= toPyObjectChecked

asTuple :: [PyObject] -> IO PyObject
asTuple objects = do
  tuple <- pyTuple_New (fromIntegral (length objects)) >>= toPyObjectChecked
  withForeignPtr tuple (setItems objects 0)
  return tuple
  where
    setItems :: [PyObject] -> PySSizeT -> RawPyObject -> IO ()
    setItems [] _ _ = return ()
    setItems (x:xs) index tuple = withForeignPtr x $ \item -> do
      pyIncRef item             -- setItem steals the reference!
      result <- pyTuple_SetItem tuple index item
      unless (result == 0) throwCurrentPythonException
      setItems xs (index + 1) tuple

asDict :: [(PyObject, PyObject)] -> IO PyObject
asDict items = do
  dict <- pyDict_New >>= toPyObjectChecked
  withForeignPtr dict (addItems items)
  return dict
  where
    addItems [] _ = return ()
    addItems ((key, value):rest) dict =
      withForeignPtr key $ \rawKey ->
      withForeignPtr value $ \rawValue -> do
      result <- pyDict_SetItem dict rawKey rawValue
      unless (result == 0) throwCurrentPythonException
      addItems rest dict

class Object a where
  toPy :: a -> IO PyObject
  fromPy :: PyObject -> IO a

instance Object [Char] where
  toPy s = useAsCStringLen (UTF8.fromString s) $ \(buffer, len) ->
    pyUnicode_FromStringAndSize buffer (fromIntegral len) >>= toPyObjectChecked
  fromPy o = do
    s <- withForeignPtr o pyUnicode_AsUTF8String >>= toPyObjectChecked
    liftM UTF8.toString (toByteString s)
    where toByteString stringObj = alloca $ \s_buffer_ptr ->
            alloca $ \s_len_ptr ->
            withForeignPtr stringObj $ \raw -> do
              result <- pyString_AsStringAndSize raw s_buffer_ptr s_len_ptr
              unless (result == 0) throwCurrentPythonException
              buffer <- peek s_buffer_ptr
              len <- peek s_len_ptr
              packCStringLen (buffer, fromIntegral len)
