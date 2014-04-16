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
{-# LANGUAGE TypeSynonymInstances #-}

module Foreign.Python
       (
         -- * Initialization
         initialize
         -- * Types
       , PyObject
         -- * Errors
       , PythonException(..)
       , isFatalError
       , isPythonException
         -- * Modules
       , importModule
         -- * Object access
       , toUnicode
       , getAttr
       , callObject
         -- * Value conversion
       , Object(..)
       )
       where

import Foreign.Python.Native

import qualified Data.ByteString.UTF8 as UTF8
import Control.Exception (Exception,throwIO)
import Control.Monad (unless,liftM,(>=>))
import Data.ByteString (ByteString,useAsCStringLen,packCStringLen)
import Data.Traversable (sequence)
import Data.Typeable (Typeable)
import Foreign.C (withCAString)
import Foreign.Ptr (nullPtr)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr,withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Prelude hiding (sequence)


newtype PyObject = PyObject (ForeignPtr ())
data PythonException = FatalPythonError
                     | PythonException String (Maybe String)
                     deriving (Typeable,Show)

instance Exception PythonException

-- |@'isFatalError' exception@ determines whether the given @exception@ denotes
-- a fatal error in the Python layer.
--
-- If a fatal error occurs, the Python layer hit an unexpected state, which
-- likely indicates a programmer error.
isFatalError :: PythonException -> Bool
isFatalError FatalPythonError = True
isFatalError _                = False

-- |@'isPythonException' exception@ determines whether the given @exception@ is
-- an exception thrown from Python code.
--
-- Unlike 'isFatalError' these exceptions can occur in the regular use of Python
-- libraries.
isPythonException :: PythonException -> Bool
isPythonException = not.isFatalError

-- |@'toPyObject' object@ converts the raw @object@ into a managed pointer.
--
-- Returns the managed pointer, or @'Nothing'@ if @object@ was a null pointer.
--
-- A managed pointer will automatically de-reference the object pointed to when
-- it goes out of scope.
toPyObject :: RawPyObject -> IO (Maybe PyObject)
toPyObject raw | raw == nullPtr  = return Nothing
toPyObject raw =
  liftM (Just . PyObject) (newForeignPtr pyDecRef raw)

-- |Like 'toPyObject', but throws the current Python exception if given a null
-- pointer.
toPyObjectChecked :: RawPyObject -> IO PyObject
toPyObjectChecked = toPyObject >=> maybe throwCurrentPythonException return

-- |@'withPyObject' object action@ runs @action@ with the unwrapped @object@.
withPyObject :: PyObject -> (RawPyObject -> IO a) -> IO a
withPyObject (PyObject ptr) = withForeignPtr ptr

-- |@'currentPythonException'@ gets the current Python exception as a triple of
-- @(excType, excValue, excTraceback)@.  If @excType@ is @Nothing@, there is no
-- current exception.  @excValue@ and @excTraceback@ may be @Nothing@ even when
-- @excType@ is not, in cause of exceptions without a value or traceback
-- respectively.
currentPythonException :: IO (Maybe PyObject, Maybe PyObject, Maybe PyObject)
currentPythonException =
  alloca $ \excTypePtr ->
  alloca $ \excValuePtr ->
  alloca $ \excTracebackPtr -> do
    pyErr_Fetch excTypePtr excValuePtr excTracebackPtr
    pyErr_NormalizeException excTypePtr excValuePtr excTracebackPtr
    excType <- peek excTypePtr >>= toPyObject
    excValue <- peek excValuePtr >>= toPyObject
    excTraceback <- peek excTracebackPtr >>= toPyObject
    return (excType, excValue, excTraceback)

-- |Throw an exception representing the current Python exception.
throwCurrentPythonException :: IO a
throwCurrentPythonException = do
  exception <- currentPythonException
  case exception of
    (Nothing, _, _) -> throwIO FatalPythonError
    (Just excType, excValue, _) -> do
      excTypeName <- qualifiedTypeName excType
      excFormattedValue <- sequence (fmap toUnicode excValue)
      throwIO (PythonException excTypeName excFormattedValue)
  where qualifiedTypeName typeObj = do
          moduleName <- getAttr typeObj "__module__"  >>= fromPy
          className <- getAttr typeObj "__name__" >>= fromPy
          return (UTF8.toString moduleName ++ "." ++ UTF8.toString className)

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

-- |@'toUnicode' object@ converts any @object@ into a unicode string, like
-- Python's @unicode()@ function does.
toUnicode :: PyObject -> IO String
toUnicode obj =
  withPyObject obj pyObject_Unicode >>= toPyObjectChecked >>= fromPy

-- |@'getAttr' object attribute@ gets the value of @attribute@ from @object@.
--
-- Throw a 'PythonException' if the attribute access failed.
getAttr :: PyObject -> String -> IO PyObject
getAttr obj attr = withPyObject obj $ \raw ->
  withCAString attr (pyObject_GetAttrString raw) >>= toPyObjectChecked

-- |@'callObject' object args kwargs@ calls a callable @object@ with the given
-- @args@ and @kwargs@.
--
-- Throw a 'PythonException' if the call failed.
callObject :: PyObject -> [PyObject] -> [(PyObject, PyObject)] -> IO PyObject
callObject obj args kwargs = do
  argsObj <- asTuple args
  kwargsObj <- asDict kwargs
  withPyObject argsObj $ \rawArgsObj ->
    withPyObject kwargsObj $ \rawKwargsObj ->
    withPyObject obj $ \raw ->
    pyObject_Call raw rawArgsObj rawKwargsObj >>= toPyObjectChecked

asTuple :: [PyObject] -> IO PyObject
asTuple objects = do
  tuple <- pyTuple_New (fromIntegral (length objects)) >>= toPyObjectChecked
  withPyObject tuple (setItems objects 0)
  return tuple
  where
    setItems :: [PyObject] -> PySSizeT -> RawPyObject -> IO ()
    setItems [] _ _ = return ()
    setItems (x:xs) index tuple = withPyObject x $ \item -> do
      pyIncRef item             -- setItem steals the reference!
      result <- pyTuple_SetItem tuple index item
      unless (result == 0) throwCurrentPythonException
      setItems xs (index + 1) tuple

asDict :: [(PyObject, PyObject)] -> IO PyObject
asDict items = do
  dict <- pyDict_New >>= toPyObjectChecked
  withPyObject dict (addItems items)
  return dict
  where
    addItems [] _ = return ()
    addItems ((key, value):rest) dict =
      withPyObject key $ \rawKey ->
      withPyObject value $ \rawValue -> do
      result <- pyDict_SetItem dict rawKey rawValue
      unless (result == 0) throwCurrentPythonException
      addItems rest dict

class Object a where
  toPy :: a -> IO PyObject
  fromPy :: PyObject -> IO a

instance Object ByteString where
  toPy s = useAsCStringLen s $ \(buffer, len) ->
    pyString_FromStringAndSize buffer (fromIntegral len) >>= toPyObjectChecked
  fromPy s =
    alloca $ \s_buffer_ptr ->
    alloca $ \s_len_ptr ->
    withPyObject s $ \raw -> do
      result <- pyString_AsStringAndSize raw s_buffer_ptr s_len_ptr
      unless (result == 0) throwCurrentPythonException
      buffer <- peek s_buffer_ptr
      len <- peek s_len_ptr
      packCStringLen (buffer, fromIntegral len)

instance Object String where
  toPy s = useAsCStringLen (UTF8.fromString s) $ \(buffer, len) ->
    pyUnicode_FromStringAndSize buffer (fromIntegral len) >>= toPyObjectChecked
  fromPy o = do
    s <- withPyObject o pyUnicode_AsUTF8String >>= toPyObjectChecked
    liftM UTF8.toString (fromPy s)
