module Nix.Bindings.Ffi where

import Data.Int (Int64)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (CBool), CDouble (CDouble), CInt (CInt), CSize (CSize), CUInt (CUInt))
import Foreign.Ptr (FunPtr, Ptr)

-- * Types

data NixCContext

data CStore

data CStorePath

data CEvalState

data CNixValue

data CPrimOp

data CExternalValue

data CListBuilder

data CBindingsBuilder

data CNixRealisedString

type NixGetStringCallback a = CString -> CInt -> Ptr a -> IO ()

type NixPrimOpFun a = Ptr a -> Ptr NixCContext -> Ptr CEvalState -> Ptr (Ptr CNixValue) -> Ptr CNixValue

-- * nix_api_util.h

foreign import ccall "nix_c_context_create"
  nixCContextCreate ::
    IO (Ptr NixCContext)

foreign import ccall "nix_c_context_free"
  nixCContextFree ::
    Ptr NixCContext -> IO ()

foreign import ccall "nix_libutil_init"
  nixLibutilInit ::
    Ptr NixCContext -> IO CInt

foreign import ccall "nix_setting_get"
  nixSettingGet ::
    Ptr NixCContext -> CString -> FunPtr (NixGetStringCallback a) -> Ptr a -> IO CInt

foreign import ccall "nix_setting_set"
  nixSettingSet ::
    Ptr NixCContext -> CString -> CString -> IO CInt

foreign import ccall "nix_version_get" nixVersionGet :: IO CString

foreign import ccall "nix_err_msg"
  nixErrMsg ::
    Ptr NixCContext -> Ptr NixCContext -> Ptr Int -> IO CString

foreign import ccall "nix_err_info_msg"
  nixErrInfoMsg ::
    Ptr NixCContext -> Ptr NixCContext -> FunPtr (NixGetStringCallback a) -> Ptr a -> IO CInt

foreign import ccall "nix_err_name"
  nixErrName ::
    Ptr NixCContext -> Ptr NixCContext -> FunPtr (NixGetStringCallback a) -> Ptr a -> IO CInt

foreign import ccall "nix_err_code"
  nixErrCode ::
    Ptr NixCContext -> IO CInt

foreign import ccall "nix_set_err_msg"
  nixSetErrMsg ::
    Ptr NixCContext -> CInt -> CString -> IO CInt

-- * nix_api_expr.h

foreign import ccall "nix_libexpr_init"
  nixLibexprInit ::
    Ptr NixCContext -> IO CInt

foreign import ccall "nix_expr_eval_from_string"
  nixExprEvalFromString ::
    Ptr NixCContext -> Ptr CEvalState -> CString -> CString -> Ptr CNixValue -> IO CInt

foreign import ccall "nix_value_call"
  nixValueCall ::
    Ptr NixCContext -> Ptr CEvalState -> Ptr CNixValue -> Ptr CNixValue -> Ptr CNixValue -> IO CInt

foreign import ccall "nix_value_call_multi"
  nixValueCallMulti ::
    Ptr NixCContext -> Ptr CEvalState -> Ptr CNixValue -> CSize -> Ptr (Ptr CNixValue) -> Ptr CNixValue -> IO CInt

foreign import ccall "nix_value_force"
  nixValueForce ::
    Ptr NixCContext -> Ptr CEvalState -> Ptr CNixValue -> IO CInt

foreign import ccall "nix_value_force_deep"
  nixValueForceDeep ::
    Ptr NixCContext -> Ptr CEvalState -> Ptr CNixValue -> IO CInt

foreign import ccall "nix_state_create"
  nixStateCreate ::
    Ptr NixCContext -> Ptr CString -> Ptr CStore -> IO (Ptr CEvalState)

foreign import ccall "nix_state_free"
  nixStateFree ::
    Ptr CEvalState -> IO ()

foreign import ccall "nix_gc_incref"
  nixGcIncref ::
    Ptr NixCContext -> Ptr a -> IO CInt

foreign import ccall "nix_gc_decref"
  nixGcDecref ::
    Ptr NixCContext -> Ptr a -> IO CInt

foreign import ccall "nix_gc_now"
  nixGcRef ::
    IO ()

foreign import ccall "nix_gc_register_finalizer"
  nixGcRegisterFinalizer ::
    Ptr obj -> Ptr cd -> FunPtr (Ptr obj -> Ptr cd -> IO ()) -> IO ()

-- * nix_api_store.h

foreign import ccall "nix_libstore_init"
  nixLibstoreInit ::
    Ptr NixCContext -> IO CInt

foreign import ccall "nix_libstore_init_no_load_config"
  nixLibstoreInitNoLoadConfig ::
    Ptr NixCContext -> IO CInt

foreign import ccall "nix_store_open"
  nixStoreOpen ::
    Ptr NixCContext -> CString -> Ptr (Ptr CString) -> IO (Ptr CStore)

foreign import ccall "nix_store_free"
  nixStoreFree ::
    Ptr CStore -> IO ()

foreign import ccall "nix_store_get_uri"
  nixStoreGetUri ::
    Ptr NixCContext -> Ptr CStore -> FunPtr (NixGetStringCallback a) -> Ptr a -> IO CInt

foreign import ccall "nix_store_parse_path"
  nixStoreParsePath ::
    Ptr NixCContext -> Ptr CStore -> CString -> IO (Ptr CStorePath)

foreign import ccall "nix_store_path_name"
  nixStorePathName ::
    Ptr CStorePath -> FunPtr (NixGetStringCallback a) -> Ptr a -> IO CInt

foreign import ccall "nix_store_path_clone"
  nixStorePathClone ::
    Ptr CStorePath -> IO (Ptr CStorePath)

foreign import ccall "nix_store_path_free"
  nixStorePathFree ::
    Ptr CStorePath -> IO ()

foreign import ccall "nix_store_is_valid_path"
  nixStoreIsValidPath ::
    Ptr NixCContext -> Ptr CStore -> Ptr CStorePath -> IO CBool

foreign import ccall "nix_store_realise"
  nixStoreRealise ::
    Ptr NixCContext -> Ptr CStore -> Ptr CStorePath -> Ptr a -> FunPtr (Ptr a -> CString -> CString -> IO ()) -> IO CInt

foreign import ccall "nix_store_get_version"
  nixStoreGetVersion ::
    Ptr NixCContext -> Ptr CStore -> FunPtr (NixGetStringCallback a) -> Ptr a -> IO CInt

-- * nix_api_value.h

foreign import ccall "nix_alloc_primop"
  nixAllocPrimop ::
    Ptr NixCContext -> FunPtr (NixPrimOpFun a) -> CInt -> CString -> Ptr CString -> CString -> Ptr a -> IO (Ptr CPrimOp)

foreign import ccall "nix_register_primop"
  nixRegisterPrimop ::
    Ptr NixCContext -> Ptr CPrimOp -> IO CInt

foreign import ccall "nix_alloc_value"
  nixAllocValue ::
    Ptr NixCContext -> Ptr CEvalState -> IO (Ptr CNixValue)

foreign import ccall "nix_value_incref"
  nixValueIncref ::
    Ptr NixCContext -> Ptr CNixValue -> IO CInt

foreign import ccall "nix_value_decref"
  nixValueDecref ::
    Ptr NixCContext -> Ptr CNixValue -> IO CInt

foreign import ccall "nix_get_type"
  nixGetType ::
    Ptr NixCContext -> Ptr CNixValue -> IO CInt

foreign import ccall "nix_get_typename"
  nixGetTypename ::
    Ptr NixCContext -> Ptr CNixValue -> IO CString

foreign import ccall "nix_get_bool"
  nixGetBool ::
    Ptr NixCContext -> Ptr CNixValue -> IO CBool

foreign import ccall "nix_get_string"
  nixGetString ::
    Ptr NixCContext -> Ptr CNixValue -> FunPtr (NixGetStringCallback a) -> Ptr a -> IO CInt

foreign import ccall "nix_get_path_string"
  nixGetPathString ::
    Ptr NixCContext -> Ptr CNixValue -> IO CString

foreign import ccall "nix_get_list_size"
  nixGetListSize ::
    Ptr NixCContext -> Ptr CNixValue -> IO CUInt

foreign import ccall "nix_get_attrs_size"
  nixGetAttrsSize ::
    Ptr NixCContext -> Ptr CNixValue -> IO CUInt

foreign import ccall "nix_get_float"
  nixGetFloat ::
    Ptr NixCContext -> Ptr CNixValue -> IO CDouble

foreign import ccall "nix_get_int"
  nixGetInt ::
    Ptr NixCContext -> Ptr CNixValue -> IO Int64

foreign import ccall "nix_get_external"
  nixGetExternal ::
    Ptr NixCContext -> Ptr CNixValue -> IO (Ptr CExternalValue)

foreign import ccall "nix_get_list_byidx"
  nixGetListByidx ::
    Ptr NixCContext -> Ptr CNixValue -> Ptr CEvalState -> CUInt -> IO (Ptr CNixValue)

foreign import ccall "nix_get_attr_byname"
  nixGetAttrByname ::
    Ptr NixCContext -> Ptr CNixValue -> Ptr CEvalState -> CString -> IO (Ptr CNixValue)

foreign import ccall "nix_get_attr_byidx"
  nixGetAttrByidx ::
    Ptr NixCContext -> Ptr CNixValue -> Ptr CEvalState -> CUInt -> Ptr CString -> IO (Ptr CNixValue)

foreign import ccall "nix_get_attr_name_byidx"
  nixGetAttrNameByidx ::
    Ptr NixCContext -> Ptr CNixValue -> Ptr CEvalState -> CUInt -> IO CString

foreign import ccall "nix_init_bool"
  nixInitBool ::
    Ptr NixCContext -> Ptr CNixValue -> CBool -> IO CInt

foreign import ccall "nix_init_string"
  nixInitString ::
    Ptr NixCContext -> Ptr CNixValue -> CString -> IO CInt

foreign import ccall "nix_init_path_string"
  nixInitPathString ::
    Ptr NixCContext -> Ptr CEvalState -> Ptr CNixValue -> CString -> IO CInt

foreign import ccall "nix_init_float"
  nixInitFloat ::
    Ptr NixCContext -> Ptr CNixValue -> CDouble -> IO CInt

foreign import ccall "nix_init_int"
  nixInitInt ::
    Ptr NixCContext -> Ptr CNixValue -> Int64 -> IO CInt

foreign import ccall "nix_init_null"
  nixInitNull ::
    Ptr NixCContext -> Ptr CNixValue -> IO CInt

foreign import ccall "nix_init_apply"
  nixInitApply ::
    Ptr NixCContext -> Ptr CNixValue -> Ptr CNixValue -> Ptr CNixValue -> IO CInt

foreign import ccall "nix_init_external"
  nixInitExternal ::
    Ptr NixCContext -> Ptr CNixValue -> Ptr CExternalValue -> IO CInt

foreign import ccall "nix_make_list"
  nixMakeList ::
    Ptr NixCContext -> Ptr CListBuilder -> Ptr CNixValue -> IO CInt

foreign import ccall "nix_make_list_builder"
  nixMakeListBuilder ::
    Ptr NixCContext -> Ptr CEvalState -> CSize -> IO (Ptr CListBuilder)

foreign import ccall "nix_list_builder_insert"
  nixListBuilderInsert ::
    Ptr NixCContext -> Ptr CListBuilder -> CUInt -> Ptr CNixValue -> IO CInt

foreign import ccall "nix_list_builder_free"
  nixListBuilderFree ::
    Ptr CListBuilder -> IO ()

foreign import ccall "nix_make_attrs"
  nixMakeAttrs ::
    Ptr NixCContext -> Ptr CNixValue -> Ptr CBindingsBuilder -> IO CInt

foreign import ccall "nix_init_primop"
  nixInitPrimop ::
    Ptr NixCContext -> Ptr CNixValue -> Ptr CPrimOp -> IO CInt

foreign import ccall "nix_copy_value"
  nixCopyValue ::
    Ptr NixCContext -> Ptr CNixValue -> Ptr CNixValue -> IO CInt

foreign import ccall "nix_make_bindings_builder"
  nixMakeBindingsBuilder ::
    Ptr NixCContext -> Ptr CEvalState -> CSize -> IO (Ptr CBindingsBuilder)

foreign import ccall "nix_bindings_builder_insert"
  nixBindingsBuilderInsert ::
    Ptr NixCContext -> Ptr CBindingsBuilder -> CString -> Ptr CNixValue -> IO CInt

foreign import ccall "nix_bindings_builder_free"
  nixBindingsBuilderFree ::
    Ptr CBindingsBuilder -> IO ()

foreign import ccall "nix_string_realise"
  nixStringRealise ::
    Ptr NixCContext -> Ptr CEvalState -> Ptr CNixValue -> CBool -> IO (Ptr CNixRealisedString)

foreign import ccall "nix_realised_string_get_buffer_start"
  nixRealisedStringGetBufferStart ::
    Ptr CNixRealisedString -> IO CString

foreign import ccall "nix_realised_string_get_buffer_size"
  nixRealisedStringGetBufferSize ::
    Ptr CNixRealisedString -> IO CSize

foreign import ccall "nix_realised_string_get_store_path_count"
  nixRealisedStringGetStorePathCount ::
    Ptr CNixRealisedString -> IO CSize

foreign import ccall "nix_realised_string_get_store_path"
  nixRealisedStringGetStorePath ::
    Ptr CNixRealisedString -> CSize -> IO (Ptr CStorePath)

foreign import ccall "nix_realised_string_free"
  nixRealisedStringFree ::
    Ptr CNixRealisedString -> IO ()
