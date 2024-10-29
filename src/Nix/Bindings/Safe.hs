module Nix.Bindings.Safe (
  -- * Initialization
  Context,
  newContext,
  EvalState,
  newEvalState,

  -- * Re-exports
  module Nix.Bindings.Safe.Store,
  module Nix.Bindings.Safe.Derivation,
  module Nix.Bindings.Safe.Builtins,
  module Nix.Bindings.Safe.Error,

  -- * Values
  ToNixValue (toNixValue),
  IsNixValue (asNixValue),
  NixValue,
  NixBool,
  getNixBool,
  newValueBool,
  getValueBool,
  NixString,
  getNixString,
  newValueString,
  getValueString,
  PathString,
  getPathString,
  NixPathString,
  getNixPathString,
  newValuePathString,
  getValuePathString,
  ContextString,
  contextStringFromText,
  contextStringFromString,
  contextStringPretty,
  newValueContextString,
  NixFloat,
  getNixFloat,
  newValueFloat,
  getValueFloat,
  NixInt,
  getNixInt,
  newValueInt,
  getValueInt,
  NixNull,
  getNixNull,
  newValueNull,
  NixList,
  getNixList,
  newValueList,
  newValueListNix,
  getValueList,
  NixAttrs,
  getNixAttrs,
  newValueAttrs,
  lookupAttr,
  (:->),
  getNixFunction,
  applyStrict,

  -- * Utils
  settingGet,
  settingSet,
  versionGet,
  writeToFile,

  -- * Errors
  NixErr (..),
  NixErrKind (..),
  NixException (..),
) where

import Data.Text (Text)
import Data.Text.Foreign qualified as Text
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import Foreign.Ptr (nullPtr)

import Nix.Bindings.Ffi
import Nix.Bindings.Safe.Builtins
import Nix.Bindings.Safe.Derivation
import Nix.Bindings.Safe.Error
import Nix.Bindings.Safe.Internal
import Nix.Bindings.Safe.Internal.ContextString
import Nix.Bindings.Safe.Internal.FfiWrappers
import Nix.Bindings.Safe.Internal.Value
import Nix.Bindings.Safe.Store

writeToFile :: Context -> EvalState -> Text -> Text -> IO ContextString
writeToFile context evalState name value = do
  toFile <- builtinToFile context evalState
  nixName <- newValueString context evalState name
  nixValue <- newValueString context evalState value
  toFile' <- applyStrict context evalState toFile nixName
  storePath <- applyStrict context evalState toFile' nixValue
  ContextStringStore <$> getValueString context storePath

newEvalState :: Context -> [Text] -> Store -> IO EvalState
newEvalState (Context context) lookupPath (Store store) =
  withForeignPtr context $ \contextPtr ->
    withForeignPtr store $ \storePtr -> do
      libexprErr <- nixLibexprInit contextPtr
      rethrowErrorCode contextPtr libexprErr
      withNullTerminatedArrayOfCString lookupPath $ \cLookupPath -> do
        statePtr <- nixStateCreate contextPtr cLookupPath storePtr
        freeState <- wrapperGetNixStateFree
        stateFPtr <- newForeignPtr freeState statePtr
        pure $ EvalState stateFPtr

newContext :: IO Context
newContext = do
  contexPtr <- nixCContextCreate
  code <- nixLibutilInit contexPtr
  rethrowErrorCode contexPtr code
  freeStore <- wrapperGetNixCContextFree
  fp <- newForeignPtr freeStore contexPtr
  pure $ Context fp

settingGet :: Context -> Text -> IO Text
settingGet (Context context) setting =
  Text.withCString setting $ \setting' ->
    withForeignPtr context $ \contextPtr ->
      captureText contextPtr (\callback -> nixSettingGet contextPtr setting' callback nullPtr)

settingSet :: Context -> Text -> Text -> IO ()
settingSet (Context context) setting value =
  Text.withCString setting $ \setting' ->
    Text.withCString value $ \value' ->
      withForeignPtr context $ \contextPtr ->
        nixSettingSet contextPtr setting' value' >>= rethrowErrorCode contextPtr

versionGet :: IO Text
versionGet = nixVersionGet >>= textPeekCString
