module Nix.Bindings.Safe.Internal where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Exception (bracket)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Foreign qualified as Text
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.C.Types (CInt (CInt))
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (withArray0)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullPtr)

import Nix.Bindings.Ffi
import Nix.Bindings.Safe.Error

foreign import ccall "wrapper"
  mkNixGetStringCallback ::
    NixGetStringCallback a -> IO (FunPtr (NixGetStringCallback a))

foreign import ccall "wrapper"
  mkNixGetRealisationResults ::
    (Ptr a -> CString -> CString -> IO ()) -> IO (FunPtr (Ptr a -> CString -> CString -> IO ()))

foreign import ccall "wrapper_get_nix_value_decref"
  wrapperGetNixValueDecref ::
    IO (FunPtr (Ptr CNixValue -> IO ()))

foreign import ccall "wrapper_get_nix_store_free"
  wrapperGetNixStoreFree ::
    IO (FunPtr (Ptr CStore -> IO ()))

foreign import ccall "wrapper_get_nix_c_context_free"
  wrapperGetNixCContextFree ::
    IO (FunPtr (Ptr NixCContext -> IO ()))

foreign import ccall "wrapper_get_nix_state_free"
  wrapperGetNixStateFree ::
    IO (FunPtr (Ptr CEvalState -> IO ()))

foreign import ccall "wrapper_get_nix_store_path_free"
  wrapperGetNixStorePathFree ::
    IO (FunPtr (Ptr CStorePath -> IO ()))

captureText :: Ptr NixCContext -> (FunPtr (NixGetStringCallback a) -> IO CInt) -> IO Text
captureText context f = do
  resM <- newEmptyMVar
  err <-
    bracket
      (mkNixGetStringCallback $ \start n _ -> Text.peekCStringLen (start, fromIntegral n) >>= putMVar resM)
      freeHaskellFunPtr
      f
  rethrowErrorCode context err
  readMVar resM

withNullTerminatedArrayOfCString :: [Text] -> (Ptr CString -> IO a) -> IO a
withNullTerminatedArrayOfCString arr f = do
  bracket
    (traverse (newCString . Text.unpack) arr)
    (traverse free)
    (\strs -> withArray0 nullPtr strs f)

textPeekCString :: CString -> IO Text
#if MIN_VERSION_text(2,1,2)
textPeekCString = Text.peekCString
#else
textPeekCString = fmap Text.pack . peekCString
#endif
