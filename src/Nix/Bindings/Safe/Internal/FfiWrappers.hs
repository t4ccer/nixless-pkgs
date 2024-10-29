module Nix.Bindings.Safe.Internal.FfiWrappers where

import Data.Text (Text)
import Foreign.ForeignPtr (ForeignPtr)

import Nix.Bindings.Ffi

data StorePath = StorePath
  { storePathPtr :: ForeignPtr CStorePath
  , storePathRaw :: Text
  }
  deriving stock (Show)

newtype Store = Store {getStore :: ForeignPtr CStore}
  deriving stock (Show)

newtype EvalState = EvalState {getEvalState :: ForeignPtr CEvalState}
  deriving stock (Show)

newtype Context = Context {getContext :: ForeignPtr NixCContext}
  deriving stock (Show)
