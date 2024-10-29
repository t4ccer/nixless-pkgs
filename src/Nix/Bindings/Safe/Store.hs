module Nix.Bindings.Safe.Store (
  -- * Stores
  Store,
  newStore,
  storeGetUri,
  storeGetVersion,

  -- * Store Paths
  StorePath,
  storePathParse,
  storePathName,
  ValidStorePath,
  getValidStorePath,
  validStorePathToContextString,
  storePathValidate,
  copyPathToStore,

  -- * Store Outputs
  Output,
  outputPath,
  outputStorePath,
  Outputs,
  getOutputs,
  storeRealise,
) where

import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Exception (bracket, throwIO)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Foreign qualified as Text
import Foreign.C.String (withCString)
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import Foreign.Ptr (freeHaskellFunPtr, nullPtr)

import Nix.Bindings.Ffi
import Nix.Bindings.Safe.Error
import Nix.Bindings.Safe.Internal
import Nix.Bindings.Safe.Internal.ContextString
import Nix.Bindings.Safe.Internal.FfiWrappers
import Nix.Bindings.Safe.Internal.Value

-- TODO: Handle params
newStore ::
  Context ->
  -- | Store URI
  Text ->
  [(Text, Text)] ->
  IO Store
newStore (Context context) uri _params =
  withForeignPtr context $ \contextPtr -> do
    libStoreErr <- nixLibstoreInit contextPtr
    rethrowErrorCode contextPtr libStoreErr
    Text.withCString uri $ \cUri -> do
      storePtr <- nixStoreOpen contextPtr cUri nullPtr
      rethrowErrorCtx contextPtr
      freeStore <- wrapperGetNixStoreFree
      storeFPtr <- newForeignPtr freeStore storePtr
      pure $ Store storeFPtr

storePathParse :: Context -> Store -> Text -> IO StorePath
storePathParse (Context context) (Store store) path =
  withForeignPtr context $ \contextPtr ->
    withForeignPtr store $ \storePtr ->
      Text.withCString path $ \pathPtr -> do
        storePathRawPtr <- nixStoreParsePath contextPtr storePtr pathPtr
        freeStorePath <- wrapperGetNixStorePathFree
        storePathFPtr <- newForeignPtr freeStorePath storePathRawPtr
        rethrowErrorCtx contextPtr
        pure $ StorePath storePathFPtr path

storePathName :: Context -> StorePath -> IO Text
storePathName (Context context) (StorePath path _) =
  withForeignPtr context $ \contextPtr ->
    withForeignPtr path $ \pathPtr ->
      captureText contextPtr (\callback -> nixStorePathName pathPtr callback nullPtr)

newtype ValidStorePath = ValidStorePath {getValidStorePath :: StorePath}
  deriving stock (Show)

validStorePathToContextString :: ValidStorePath -> ContextString
validStorePathToContextString = ContextStringStore . storePathRaw . getValidStorePath

storePathValidate :: Context -> Store -> StorePath -> IO ValidStorePath
storePathValidate (Context context) (Store store) (StorePath path rawPath) =
  withForeignPtr context $ \contextPtr ->
    withForeignPtr store $ \storePtr ->
      withForeignPtr path $ \pathPtr -> do
        isValid <- nixStoreIsValidPath contextPtr storePtr pathPtr
        if isValid /= 0
          then pure $ ValidStorePath $ StorePath path rawPath
          else throwIO NixExceptionInvalidStorePath

data Output = Output
  { outputPath :: Text
  , outputStorePath :: ValidStorePath
  }
  deriving stock (Show)

newtype Outputs = Outputs {getOutputs :: Map Text Output}
  deriving stock (Show)

storeRealise :: Context -> Store -> ValidStorePath -> IO (Maybe Outputs)
storeRealise (Context context) (Store store) (ValidStorePath (StorePath path _)) =
  withForeignPtr context $ \contextPtr ->
    withForeignPtr store $ \storePtr ->
      withForeignPtr path $ \pathPtr -> do
        resM <- newMVar Map.empty
        code <-
          bracket
            ( mkNixGetRealisationResults $ \_ outname out -> do
                outNameStr <- textPeekCString outname
                outStr <- textPeekCString out
                storePath <- storePathParse (Context context) (Store store) outStr
                storePathValid <- storePathValidate (Context context) (Store store) storePath
                modifyMVar_ resM (pure . Map.insert outNameStr (Output outStr storePathValid))
            )
            freeHaskellFunPtr
            (nixStoreRealise contextPtr storePtr pathPtr nullPtr)
        rethrowErrorCode contextPtr code
        res <- readMVar resM
        -- There is no error code on failed builid
        if Map.null res
          then pure Nothing
          else pure $ Just $ Outputs res

storeGetUri :: Context -> Store -> IO Text
storeGetUri (Context context) (Store store) =
  withForeignPtr context $ \contextPtr ->
    withForeignPtr store $ \storePtr ->
      captureText contextPtr (\callback -> nixStoreGetUri contextPtr storePtr callback nullPtr)

storeGetVersion :: Context -> Store -> IO Text
storeGetVersion (Context context) (Store store) =
  withForeignPtr context $ \contextPtr ->
    withForeignPtr store $ \storePtr ->
      captureText contextPtr (\callback -> nixStoreGetVersion contextPtr storePtr callback nullPtr)

copyPathToStore :: Context -> EvalState -> Store -> FilePath -> FilePath -> IO ValidStorePath
copyPathToStore (Context context) (EvalState evalState) store cwd fp = do
  withForeignPtr context $ \contextPtr ->
    withForeignPtr evalState $ \evalStatePtr ->
      withCString ("\"${./" <> fp <> "}\"") $ \exprPtr ->
        withCString cwd $ \cwdPtr -> do
          value <- newValueEmpty (Context context) (EvalState evalState)
          withForeignPtr (getNixValue value) $ \valuePtr -> do
            code <- nixExprEvalFromString contextPtr evalStatePtr exprPtr cwdPtr valuePtr
            rethrowErrorCode contextPtr code
            path <- getValueString (Context context) $ unsafeFromNixValue value
            storePath <- storePathParse (Context context) store path
            storePathValidate (Context context) store storePath
