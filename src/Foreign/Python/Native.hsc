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

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module Foreign.Python.Native where

#include <Python.h>

import Data.Int (Int32,Int64)
import Foreign.C (CString)
import Foreign.Ptr (Ptr,FunPtr)

type RawPyObject = Ptr ()
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

foreign import capi unsafe "Python.h PyUnicode_AsUTF8String"
  pyUnicode_AsUTF8String :: RawPyObject -> IO RawPyObject

foreign import capi unsafe "Python.h PyUnicode_FromStringAndSize"
  pyUnicode_FromStringAndSize :: CString -> PySSizeT -> IO RawPyObject

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

foreign import ccall unsafe "Python.h PyErr_PrintEx"
  pyErr_PrintEx :: PyInt -> IO ()

foreign import ccall unsafe "Python.h PyErr_Occurred"
  pyErr_Occurred :: IO RawPyObject
