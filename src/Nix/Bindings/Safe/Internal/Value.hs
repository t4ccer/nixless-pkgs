module Nix.Bindings.Safe.Internal.Value where

import Control.Exception (bracket)
import Control.Monad (forM, forM_)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Foreign qualified as Text
import Foreign (alloca, peek)
import Foreign.C.Types (CBool (CBool), CDouble (CDouble))
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, nullPtr)

import Nix.Bindings.Ffi
import Nix.Bindings.Safe.Error
import Nix.Bindings.Safe.Internal
import Nix.Bindings.Safe.Internal.FfiWrappers

class ToNixValue nix where
  toNixValue :: nix -> NixValue
  unsafeFromNixValue :: NixValue -> nix

class ToNixValue nix => IsNixValue haskell nix | haskell -> nix where
  asNixValue :: Context -> EvalState -> haskell -> IO nix
  fromNixValue :: Context -> EvalState -> nix -> IO haskell

newtype NixValue = NixValue {getNixValue :: ForeignPtr CNixValue}
  deriving stock (Show)

instance ToNixValue NixValue where
  toNixValue = id
  unsafeFromNixValue = id

instance IsNixValue NixValue NixValue where
  asNixValue _ _ = pure
  fromNixValue _ _ = pure

wrapValuePtr :: Ptr CNixValue -> IO NixValue
wrapValuePtr valuePtr = do
  decref <- wrapperGetNixValueDecref
  NixValue <$> newForeignPtr decref valuePtr

newValueEmpty :: Context -> EvalState -> IO NixValue
newValueEmpty (Context context) (EvalState evalState) =
  withForeignPtr context $ \contextPtr ->
    withForeignPtr evalState $ \evalStatePtr -> do
      valuePtr <- nixAllocValue contextPtr evalStatePtr
      rethrowErrorCtx contextPtr
      wrapValuePtr valuePtr

newtype NixBool = NixBool {getNixBool :: NixValue}
  deriving stock (Show)
  deriving newtype (ToNixValue)

instance IsNixValue Bool NixBool where
  asNixValue = newValueBool
  fromNixValue context _ = getValueBool context

newValueBool :: Context -> EvalState -> Bool -> IO NixBool
newValueBool (Context context) (EvalState evalState) bool = do
  value <- newValueEmpty (Context context) (EvalState evalState)
  withForeignPtr context $ \contextPtr ->
    withForeignPtr (getNixValue value) $ \valuePtr -> do
      code <- nixInitBool contextPtr valuePtr (CBool (if bool then 0 else 1))
      rethrowErrorCode contextPtr code
      pure $ NixBool value

getValueBool :: Context -> NixBool -> IO Bool
getValueBool (Context context) nixBool =
  withForeignPtr context $ \contextPtr ->
    withForeignPtr (getNixValue $ getNixBool nixBool) $ \valuePtr -> do
      (/= 0) <$> nixGetBool contextPtr valuePtr

newtype NixString = NixString {getNixString :: NixValue}
  deriving stock (Show)
  deriving newtype (ToNixValue)

instance IsNixValue Text NixString where
  asNixValue = newValueString
  fromNixValue context _ = getValueString context

newValueString :: Context -> EvalState -> Text -> IO NixString
newValueString (Context context) (EvalState evalState) str = do
  value <- newValueEmpty (Context context) (EvalState evalState)
  withForeignPtr context $ \contextPtr ->
    withForeignPtr (getNixValue value) $ \valuePtr ->
      Text.withCString str $ \cStr -> do
        code <- nixInitString contextPtr valuePtr cStr
        rethrowErrorCode contextPtr code
        pure $ NixString value

getValueString :: Context -> NixString -> IO Text
getValueString (Context context) nixStr =
  withForeignPtr context $ \contextPtr ->
    withForeignPtr (getNixValue $ getNixString nixStr) $ \valuePtr ->
      captureText contextPtr (\callback -> nixGetString contextPtr valuePtr callback nullPtr)

newtype PathString = PathString {getPathString :: Text}
  deriving stock (Show)
  deriving newtype (IsString)

newtype NixPathString = NixPathString {getNixPathString :: NixValue}
  deriving stock (Show)
  deriving newtype (ToNixValue)

instance IsNixValue PathString NixPathString where
  asNixValue = newValuePathString
  fromNixValue context _ = getValuePathString context

newValuePathString :: Context -> EvalState -> PathString -> IO NixPathString
newValuePathString (Context context) (EvalState evalState) (PathString str) = do
  value <- newValueEmpty (Context context) (EvalState evalState)
  withForeignPtr context $ \contextPtr ->
    withForeignPtr evalState $ \evalStatePtr ->
      withForeignPtr (getNixValue value) $ \valuePtr ->
        Text.withCString str $ \cStr -> do
          code <- nixInitPathString contextPtr evalStatePtr valuePtr cStr
          rethrowErrorCode contextPtr code
          pure $ NixPathString value

getValuePathString :: Context -> NixPathString -> IO PathString
getValuePathString (Context context) nixPathStr =
  withForeignPtr context $ \contextPtr ->
    withForeignPtr (getNixValue $ getNixPathString nixPathStr) $ \valuePtr ->
      nixGetPathString contextPtr valuePtr >>= fmap PathString . textPeekCString

newtype NixFloat = NixFloat {getNixFloat :: NixValue}
  deriving stock (Show)
  deriving newtype (ToNixValue)

instance IsNixValue Double NixFloat where
  asNixValue = newValueFloat
  fromNixValue context _ = getValueFloat context

newValueFloat :: Context -> EvalState -> Double -> IO NixFloat
newValueFloat (Context context) (EvalState evalState) double = do
  value <- newValueEmpty (Context context) (EvalState evalState)
  withForeignPtr context $ \contextPtr ->
    withForeignPtr (getNixValue value) $ \valuePtr -> do
      code <- nixInitFloat contextPtr valuePtr (CDouble double)
      rethrowErrorCode contextPtr code
      pure $ NixFloat value

getValueFloat :: Context -> NixFloat -> IO Double
getValueFloat (Context context) nixFloat =
  withForeignPtr context $ \contextPtr ->
    withForeignPtr (getNixValue $ getNixFloat nixFloat) $ \valuePtr ->
      nixGetFloat contextPtr valuePtr >>= \case
        CDouble double -> pure double

newtype NixInt = NixInt {getNixInt :: NixValue}
  deriving stock (Show)
  deriving newtype (ToNixValue)

instance IsNixValue Int64 NixInt where
  asNixValue = newValueInt
  fromNixValue context _ = getValueInt context

newValueInt :: Context -> EvalState -> Int64 -> IO NixInt
newValueInt (Context context) (EvalState evalState) int = do
  value <- newValueEmpty (Context context) (EvalState evalState)
  withForeignPtr context $ \contextPtr ->
    withForeignPtr (getNixValue value) $ \valuePtr -> do
      code <- nixInitInt contextPtr valuePtr int
      rethrowErrorCode contextPtr code
      pure $ NixInt value

getValueInt :: Context -> NixInt -> IO Int64
getValueInt (Context context) nixInt =
  withForeignPtr context $ \contextPtr ->
    withForeignPtr (getNixValue $ getNixInt nixInt) $ \valuePtr ->
      nixGetInt contextPtr valuePtr

newtype NixNull = NixNull {getNixNull :: NixValue}
  deriving stock (Show)
  deriving newtype (ToNixValue)

instance IsNixValue () NixNull where
  asNixValue context evalState () = newValueNull context evalState
  fromNixValue _ _ _ = pure ()

newValueNull :: Context -> EvalState -> IO NixNull
newValueNull (Context context) (EvalState evalState) = do
  value <- newValueEmpty (Context context) (EvalState evalState)
  withForeignPtr context $ \contextPtr ->
    withForeignPtr (getNixValue value) $ \valuePtr -> do
      code <- nixInitNull contextPtr valuePtr
      rethrowErrorCode contextPtr code
      pure $ NixNull value

type role NixList nominal
newtype NixList a = NixList {getNixList :: NixValue}
  deriving stock (Show)
  deriving newtype (ToNixValue)

instance IsNixValue haskell nix => IsNixValue [haskell] (NixList nix) where
  asNixValue = newValueList
  fromNixValue = getValueList

newValueList ::
  IsNixValue haskell nix =>
  Context ->
  EvalState ->
  [haskell] ->
  IO (NixList nix)
newValueList (Context context) (EvalState evalState) lst =
  withForeignPtr context $ \contextPtr ->
    withForeignPtr evalState $ \evalStatePtr ->
      bracket
        (nixMakeListBuilder contextPtr evalStatePtr (fromIntegral $ length lst))
        nixListBuilderFree
        $ \builder -> do
          forM_ (zip [0 ..] lst) $ \(idx, val) -> do
            thisValue <- toNixValue <$> asNixValue (Context context) (EvalState evalState) val
            withForeignPtr (getNixValue thisValue) $ \thisValuePtr ->
              nixListBuilderInsert contextPtr builder idx thisValuePtr
          nixRet <- newValueEmpty (Context context) (EvalState evalState)
          withForeignPtr (getNixValue nixRet) $ \retPtr -> do
            code <- nixMakeList contextPtr builder retPtr
            rethrowErrorCode contextPtr code
          pure $ NixList nixRet

newValueListNix ::
  ToNixValue nix =>
  Context ->
  EvalState ->
  [nix] ->
  IO (NixList nix)
newValueListNix (Context context) (EvalState evalState) lst =
  withForeignPtr context $ \contextPtr ->
    withForeignPtr evalState $ \evalStatePtr ->
      bracket
        (nixMakeListBuilder contextPtr evalStatePtr (fromIntegral $ length lst))
        nixListBuilderFree
        $ \builder -> do
          forM_ (zip [0 ..] lst) $ \(idx, val) -> do
            withForeignPtr (getNixValue $ toNixValue val) $ \thisValuePtr ->
              nixListBuilderInsert contextPtr builder idx thisValuePtr
          nixRet <- newValueEmpty (Context context) (EvalState evalState)
          withForeignPtr (getNixValue nixRet) $ \retPtr -> do
            code <- nixMakeList contextPtr builder retPtr
            rethrowErrorCode contextPtr code
          pure $ NixList nixRet

getValueList ::
  forall haskell nix.
  IsNixValue haskell nix =>
  Context ->
  EvalState ->
  NixList nix ->
  IO [haskell]
getValueList (Context context) (EvalState evalState) nixList = do
  withForeignPtr context $ \contextPtr ->
    withForeignPtr evalState $ \evalStatePtr ->
      withForeignPtr (getNixValue $ getNixList nixList) $ \nixListPtr -> do
        listSize <- nixGetListSize contextPtr nixListPtr
        rethrowErrorCtx contextPtr
        forM [0 .. listSize - 1] $ \idx -> do
          valuePtr <- nixGetListByidx contextPtr nixListPtr evalStatePtr idx
          valueNix :: nix <- unsafeFromNixValue <$> wrapValuePtr valuePtr
          fromNixValue (Context context) (EvalState evalState) valueNix

newtype NixAttrs = NixAttrs {getNixAttrs :: NixValue}
  deriving stock (Show)
  deriving newtype (ToNixValue)

instance IsNixValue (Map Text NixValue) NixAttrs where
  asNixValue = newValueAttrs
  fromNixValue = getValueAttrs

newValueAttrs :: Context -> EvalState -> Map Text NixValue -> IO NixAttrs
newValueAttrs (Context context) (EvalState evalState) attrs = do
  withForeignPtr context $ \contextPtr ->
    withForeignPtr evalState $ \evalStatePtr ->
      bracket
        (nixMakeBindingsBuilder contextPtr evalStatePtr (fromIntegral $ Map.size attrs))
        nixBindingsBuilderFree
        $ \builder -> do
          forM_ (Map.toList attrs) $ \(key, val) -> do
            thisVal <- toNixValue <$> asNixValue (Context context) (EvalState evalState) val
            withForeignPtr (getNixValue thisVal) $ \thisValPtr ->
              Text.withCString key $ \keyPtr ->
                nixBindingsBuilderInsert contextPtr builder keyPtr thisValPtr
          nixRet <- newValueEmpty (Context context) (EvalState evalState)
          withForeignPtr (getNixValue nixRet) $ \retPtr -> do
            code <- nixMakeAttrs contextPtr retPtr builder
            rethrowErrorCode contextPtr code
          pure $ NixAttrs nixRet

getValueAttrs ::
  Context ->
  EvalState ->
  NixAttrs ->
  IO (Map Text NixValue)
getValueAttrs (Context context) (EvalState evalState) nixAttrs = do
  withForeignPtr context $ \contextPtr ->
    withForeignPtr evalState $ \evalStatePtr ->
      withForeignPtr (getNixValue $ getNixAttrs nixAttrs) $ \nixAttrsPtr -> do
        attrCount <- nixGetAttrsSize contextPtr nixAttrsPtr
        rethrowErrorCtx contextPtr
        tuples <- forM [0 .. attrCount - 1] $ \idx -> do
          alloca $ \namePtr -> do
            valuePtr <- nixGetAttrByidx contextPtr nixAttrsPtr evalStatePtr idx namePtr
            v <- wrapValuePtr valuePtr
            nameCStr <- peek namePtr
            name <- textPeekCString nameCStr
            pure (name, v)
        pure $ Map.fromList tuples

lookupAttr :: Context -> EvalState -> Text -> NixAttrs -> IO NixValue
lookupAttr (Context context) (EvalState evalState) key attrs = do
  withForeignPtr context $ \contextPtr ->
    withForeignPtr evalState $ \evalStatePtr ->
      Text.withCString key $ \keyPtr ->
        withForeignPtr (getNixValue $ getNixAttrs attrs) $ \attrsPtr -> do
          valPtr <- nixGetAttrByname contextPtr attrsPtr evalStatePtr keyPtr
          rethrowErrorCtx contextPtr
          wrapValuePtr valPtr

newtype a :-> b = NixFunction {getNixFunction :: NixValue}
  deriving stock (Show)
  deriving newtype (ToNixValue)
infixr 0 :->

applyStrict ::
  (ToNixValue nixA, ToNixValue nixB) =>
  Context ->
  EvalState ->
  (nixA :-> nixB) ->
  nixA ->
  IO nixB
applyStrict (Context context) (EvalState evalState) nixFun nixArg =
  withForeignPtr context $ \contextPtr ->
    withForeignPtr evalState $ \evalStatePtr ->
      withForeignPtr (getNixValue $ toNixValue nixFun) $ \funPtr ->
        withForeignPtr (getNixValue $ toNixValue nixArg) $ \argPtr -> do
          nixRet <- newValueEmpty (Context context) (EvalState evalState)
          withForeignPtr (getNixValue nixRet) $ \retPtr -> do
            code <- nixValueCall contextPtr evalStatePtr funPtr argPtr retPtr
            rethrowErrorCode contextPtr code
            pure $ unsafeFromNixValue nixRet
