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
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}

module Web.Lunarsite.Python where

#include <Python.h>

import qualified Data.ByteString.UTF8 as UTF8

import Control.Monad (when,unless,liftM)
import Data.ByteString (useAsCStringLen,packCStringLen)
import Data.Int (Int32,Int64)
import Foreign.C (CString,withCAString)
import Foreign.Ptr (Ptr,FunPtr,nullPtr)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr,withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

type RawPyObject = Ptr ()
type PyObject = ForeignPtr ()
type PyInt = #type int
type PySSizeT = #type Py_ssize_t

foreign import ccall unsafe "Python.h &Py_DecRef"
  pyDecRef :: FunPtr (RawPyObject -> IO ())

foreign import ccall unsafe "Python.h Py_IncRef"
  pyIncRef :: RawPyObject -> IO ()

foreign import ccall unsafe "Python.h PyImport_ImportModule"
  pyImport_ImportModule :: CString -> IO RawPyObject

foreign import ccall unsafe "Python.h Py_InitializeEx"
  pyInitializeEx :: PyInt -> IO ()

foreign import ccall unsafe "Python.h PyString_FromStringAndSize"
  pyString_FromStringAndSize :: CString -> PySSizeT -> IO RawPyObject

foreign import ccall unsafe "Python.h PyString_AsStringAndSize"
  pyString_AsStringAndSize :: RawPyObject -> Ptr CString -> Ptr PySSizeT -> IO PyInt

#ifdef PYTHON_UCS2
foreign import ccall unsafe "Python.h PyUnicodeUCS2_AsUTF8String"
  pyUnicode_AsUTF8String :: RawPyObject -> IO RawPyObject
#else
foreign import ccall unsafe "Python.h PyUnicodeUCS4_AsUTF8String"
  pyUnicode_AsUTF8String :: RawPyObject -> IO RawPyObject
#endif

foreign import ccall unsafe "Python.h PyTuple_New"
  pyTuple_New :: PySSizeT -> IO RawPyObject

foreign import ccall unsafe "Python.h PyTuple_SetItem"
  pyTuple_SetItem :: RawPyObject -> PySSizeT -> RawPyObject -> IO PyInt

foreign import ccall unsafe "Python.h PyDict_New"
  pyDict_New :: IO RawPyObject

foreign import ccall unsafe "Python.h PyDict_SetItem"
  pyDict_SetItem :: RawPyObject -> RawPyObject -> RawPyObject -> IO PyInt

foreign import ccall unsafe "Python.h PyObject_Call"
  pyObject_Call :: RawPyObject -> RawPyObject -> RawPyObject -> IO RawPyObject

foreign import ccall unsafe "Python.h PyObject_GetAttrString"
  pyObject_GetAttrString :: RawPyObject -> CString -> IO RawPyObject

toPyObject :: RawPyObject -> IO PyObject
toPyObject = newForeignPtr pyDecRef

toPyObjectChecked :: RawPyObject -> IO PyObject
toPyObjectChecked obj = do
  when (obj == nullPtr) throwCurrentPythonException
  toPyObject obj

throwCurrentPythonException :: IO ()
throwCurrentPythonException = undefined

initialize :: Bool -> IO ()
initialize True = pyInitializeEx 1
initialize False = pyInitializeEx 0

importModule :: String -> IO PyObject
importModule modName =
  withCAString modName pyImport_ImportModule >>= toPyObjectChecked

getAttr :: PyObject -> String -> IO PyObject
getAttr obj attr = withForeignPtr obj $ \raw ->
  withCAString attr (pyObject_GetAttrString raw) >>= toPyObjectChecked

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

instance Object UTF8.ByteString where
  toPy s = useAsCStringLen s $ \(buffer, len) ->
     pyString_FromStringAndSize buffer (fromIntegral len) >>= toPyObjectChecked
  fromPy o = alloca $ \s_buffer_ptr ->
    alloca $ \s_len_ptr ->
    withForeignPtr o $ \raw -> do
      result <- pyString_AsStringAndSize raw s_buffer_ptr s_len_ptr
      unless (result == 0) throwCurrentPythonException
      buffer <- peek s_buffer_ptr
      len <- peek s_len_ptr
      packCStringLen (buffer, fromIntegral len)

instance Object [Char] where
  toPy s = toPy (UTF8.fromString s)
  fromPy o = do
    s <- withForeignPtr o pyUnicode_AsUTF8String >>= toPyObjectChecked
    liftM UTF8.toString (fromPy s)
