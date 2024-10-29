module Nix.Bindings.Safe.Derivation (
  Derivation,
  derivationDrvPath,
  derivationOutPath,
  derivation,
  realiseDerivation,
) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Nix.Bindings.Safe.Builtins
import Nix.Bindings.Safe.Internal.ContextString
import Nix.Bindings.Safe.Internal.FfiWrappers
import Nix.Bindings.Safe.Internal.Value
import Nix.Bindings.Safe.Store

data Derivation = Derivation
  { derivationDrvPath :: ValidStorePath
  , derivationOutPath :: ContextString
  }
  deriving stock (Show)

derivation ::
  Context ->
  EvalState ->
  Store ->
  -- | System
  Text ->
  -- | Name
  Text ->
  -- | Builder
  ContextString ->
  -- | Arguments to builder
  [ContextString] ->
  IO Derivation
derivation context evalState store system name builder args = do
  derivation' <- builtinDerivationRaw context evalState

  nixSystem <- newValueString context evalState system
  nixName <- newValueString context evalState name
  nixBuilder <- newValueContextString context evalState builder
  argList <- traverse (newValueContextString context evalState) args
  nixArgs <- newValueListNix context evalState argList

  drvAttrs <-
    newValueAttrs context evalState $
      Map.fromList
        [ ("name", toNixValue nixName)
        , ("system", toNixValue nixSystem)
        , ("builder", toNixValue nixBuilder)
        , ("args", toNixValue nixArgs)
        ]
  drvResAttrs <- applyStrict context evalState derivation' drvAttrs
  drvPath <- lookupAttr context evalState "drvPath" drvResAttrs >>= getValueString context . unsafeFromNixValue
  outPath <- lookupAttr context evalState "outPath" drvResAttrs >>= getValueString context . unsafeFromNixValue
  drvPathStore <- storePathParse context store drvPath >>= storePathValidate context store
  pure $ Derivation drvPathStore (ContextStringStore outPath)

realiseDerivation :: Context -> Store -> Derivation -> IO (Maybe Outputs)
realiseDerivation context store drv =
  storeRealise context store $ derivationDrvPath drv
