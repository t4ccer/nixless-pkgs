module Nix.Bindings.Safe.Internal.ContextString where

import Data.String (IsString (fromString))
import Data.Text (Text)

import Nix.Bindings.Safe.Builtins
import Nix.Bindings.Safe.Internal.FfiWrappers
import Nix.Bindings.Safe.Internal.Value

data ContextString
  = ContextStringPlain Text
  | ContextStringStore Text
  | ContextStringRope ContextString ContextString
  deriving stock (Show)

instance IsString ContextString where
  fromString = ContextStringPlain . fromString

instance Semigroup ContextString where
  (ContextStringPlain p1) <> (ContextStringPlain p2) =
    ContextStringPlain (p1 <> p2)
  (ContextStringPlain p1) <> (ContextStringStore s2) =
    ContextStringRope (ContextStringPlain p1) (ContextStringStore s2)
  (ContextStringStore s1) <> (ContextStringPlain p2) =
    ContextStringRope (ContextStringStore s1) (ContextStringPlain p2)
  (ContextStringStore s1) <> (ContextStringStore s2) =
    ContextStringRope (ContextStringStore s1) (ContextStringStore s2)
  lhs@(ContextStringRope _ _) <> rhs = ContextStringRope lhs rhs
  lhs <> rhs@(ContextStringRope _ _) = ContextStringRope lhs rhs

instance Monoid ContextString where
  mempty = ContextStringPlain ""

contextStringPretty :: ContextString -> Text
contextStringPretty (ContextStringPlain p) = p
contextStringPretty (ContextStringStore s) = s
contextStringPretty (ContextStringRope rhs lhs) = contextStringPretty rhs <> contextStringPretty lhs

contextStringFromText :: Text -> ContextString
contextStringFromText = ContextStringPlain

contextStringFromString :: String -> ContextString
contextStringFromString = fromString

newValueContextString :: Context -> EvalState -> ContextString -> IO NixString
newValueContextString context evalState (ContextStringPlain plain) = newValueString context evalState plain
newValueContextString context evalState (ContextStringStore store) = do
  storePath <- builtinStorePath context evalState
  str <- newValueString context evalState store
  applyStrict context evalState storePath str
newValueContextString context evalState (ContextStringRope lhs rhs) = do
  lhsNix <- newValueContextString context evalState lhs
  rhsNix <- newValueContextString context evalState rhs
  empty <- newValueString context evalState ""
  concatStringsSep <- builtinConcatStringsSep context evalState
  concatStringsSep' <- applyStrict context evalState concatStringsSep empty
  lst <- newValueListNix context evalState [lhsNix, rhsNix]
  applyStrict context evalState concatStringsSep' lst
