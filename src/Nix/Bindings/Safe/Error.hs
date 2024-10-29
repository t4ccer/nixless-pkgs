module Nix.Bindings.Safe.Error where

import Control.Exception (Exception, throwIO)
import Data.Text (Text)
import Data.Text.Foreign qualified as Text
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)

import Nix.Bindings.Ffi

data NixErrKind
  = NixErrKindUnknown
  | NixErrKindOverflow
  | NixErrKindKey
  | NixErrKindError
  deriving stock (Show)

nixErrKindFromC :: CInt -> Maybe NixErrKind
nixErrKindFromC 0 = Nothing
nixErrKindFromC (-1) = Just NixErrKindUnknown
nixErrKindFromC (-2) = Just NixErrKindOverflow
nixErrKindFromC (-3) = Just NixErrKindKey
nixErrKindFromC (-4) = Just NixErrKindError
nixErrKindFromC invalid = error $ "nixErrFromC: unreachable: invalid nix_err value: " <> show invalid

data NixErr = NixErr NixErrKind Text
  deriving stock (Show)

data NixException
  = NixExceptionErr NixErr
  | NixExceptionInvalidStorePath
  deriving stock (Show)
  deriving anyclass (Exception)

mkNixErrIO :: Ptr NixCContext -> IO CInt -> IO (Maybe NixErr)
mkNixErrIO context errIO = do
  err <- errIO
  case nixErrKindFromC err of
    Nothing -> pure Nothing
    Just errKind -> alloca $ \lenPtr -> do
      start <- nixErrMsg context context lenPtr
      len <- peek lenPtr
      msg <- Text.peekCStringLen (start, len)
      pure $ Just $ NixErr errKind msg

rethrowErrorCtx :: Ptr NixCContext -> IO ()
rethrowErrorCtx context = do
  maybeErr <- mkNixErrIO context $ nixErrCode context
  case maybeErr of
    Nothing -> pure ()
    Just err -> throwIO $ NixExceptionErr err

rethrowErrorCode :: Ptr NixCContext -> CInt -> IO ()
rethrowErrorCode context code = do
  maybeErr <- mkNixErrIO context $ pure code
  case maybeErr of
    Nothing -> pure ()
    Just err -> throwIO $ NixExceptionErr err
