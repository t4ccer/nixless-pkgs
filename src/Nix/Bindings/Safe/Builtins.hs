module Nix.Bindings.Safe.Builtins (
  builtinAbort,
  builtinAdd,
  builtinAll,
  builtinDerivationRaw,
  builtinStringLength,
  builtinLength,
  builtinStorePath,
  builtinToFile,
  builtinMap,
  builtinTypeOf,
  builtinConcatStringsSep,
)
where

import Foreign.C.String (withCString)
import Foreign.ForeignPtr (withForeignPtr)

import Nix.Bindings.Ffi
import Nix.Bindings.Safe.Error
import Nix.Bindings.Safe.Internal.FfiWrappers
import Nix.Bindings.Safe.Internal.Value

builtinUnsafe :: ToNixValue nix => String -> Context -> EvalState -> IO nix
builtinUnsafe builtin (Context context) (EvalState evalState) =
  withForeignPtr context $ \contextPtr ->
    withForeignPtr evalState $ \evalStatePtr ->
      withCString builtin $ \cBuiltin ->
        withCString "." $ \path -> do
          value <- newValueEmpty (Context context) (EvalState evalState)
          withForeignPtr (getNixValue value) $ \valuePtr -> do
            code <- nixExprEvalFromString contextPtr evalStatePtr cBuiltin path valuePtr
            rethrowErrorCode contextPtr code
            pure $ unsafeFromNixValue value

builtinAbort :: ToNixValue nix => Context -> EvalState -> IO nix
builtinAbort = builtinUnsafe "builtins.abort"

builtinAdd :: Context -> EvalState -> IO (NixInt :-> NixInt :-> NixInt)
builtinAdd = builtinUnsafe "builtins.add"

builtinAll :: Context -> EvalState -> IO ((a :-> NixBool) :-> NixList a :-> NixBool)
builtinAll = builtinUnsafe "builtins.all"

builtinDerivationRaw :: Context -> EvalState -> IO (NixAttrs :-> NixAttrs)
builtinDerivationRaw = builtinUnsafe "builtins.derivation"

builtinStringLength :: Context -> EvalState -> IO (NixString :-> NixInt)
builtinStringLength = builtinUnsafe "builtins.stringLength"

builtinLength :: Context -> EvalState -> IO (NixList a :-> NixInt)
builtinLength = builtinUnsafe "builtins.length"

builtinStorePath :: Context -> EvalState -> IO (NixString :-> NixString)
builtinStorePath = builtinUnsafe "builtins.storePath"

builtinToFile :: Context -> EvalState -> IO (NixString :-> NixString :-> NixString)
builtinToFile = builtinUnsafe "builtins.toFile"

builtinMap :: Context -> EvalState -> IO ((a :-> b) :-> NixList a :-> NixList b)
builtinMap = builtinUnsafe "builtins.map"

builtinTypeOf :: Context -> EvalState -> IO (a :-> NixString)
builtinTypeOf = builtinUnsafe "builtins.typeOf"

builtinConcatStringsSep :: Context -> EvalState -> IO (NixString :-> NixList NixString :-> NixString)
builtinConcatStringsSep = builtinUnsafe "builtins.concatStringsSep"
