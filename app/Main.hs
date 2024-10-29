module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT), ask)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import System.Directory (getCurrentDirectory)

import Nix.Bindings.Safe

data Env = Env
  { envRoot :: FilePath
  , envContext :: Context
  , evnStore :: Store
  , envEvalState :: EvalState
  , envSystem :: Text
  }

newtype Pkgs a = Pkgs {runPkgs :: Env -> IO a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO) via (ReaderT Env IO)

intercalate' :: Monoid m => m -> [m] -> m
intercalate' _ [] = mempty
intercalate' _ [m] = m
intercalate' x (m : ms) = m <> x <> intercalate' x ms

copyPath :: FilePath -> Pkgs ContextString
copyPath fp = do
  (Env root context store evalState _) <- ask
  validStorePathToContextString <$> liftIO (copyPathToStore context evalState store root fp)

mkTccScriptDerivation :: Text -> ContextString -> Pkgs Derivation
mkTccScriptDerivation name script = do
  (Env _ context store evalState system) <- ask
  tcc <- copyPath "./bootstrap/tcc"
  lib <- copyPath "./bootstrap/lib"
  let args = ["-nostdinc", "-nostdlib", "-I" <> lib, "-run", script]
  liftIO $ derivation context evalState store system name tcc args

mkTccStage0 :: Text -> Text -> NonEmpty ContextString -> Pkgs Derivation
mkTccStage0 name output srcs = do
  (Env _ context store evalState system) <- ask
  tcc <- copyPath "./bootstrap/tcc"
  lib <- copyPath "./bootstrap/lib"
  driver <- copyPath "./bootstrap/compiler_stage0.c"
  let args =
        [ "-nostdinc"
        , "-nostdlib"
        , "-I" <> lib
        , "-DNIXLESS_PKGS_TCC_LIB=\"" <> lib <> "\""
        , "-DNIXLESS_PKGS_TCC_PATH=\"" <> tcc <> "\""
        , "-DNIXLESS_PKGS_OUT_PATH=\"" <> contextStringFromText output <> "\""
        , "-DNIXLESS_PKGS_SRCS="
            <> intercalate'
              "."
              ( map (\s -> "\"" <> s <> "\"") $
                  NonEmpty.toList srcs
              )
        , "-run"
        , driver
        ]
  liftIO $ derivation context evalState store system name tcc args

realiseDerivation' :: Derivation -> Pkgs Outputs
realiseDerivation' drv = do
  (Env _ context store _ _) <- ask
  liftIO $
    realiseDerivation context store drv >>= \case
      Just outputs -> pure outputs
      Nothing -> error ("Failed to build:\n" <> show drv)

main :: IO ()
main = do
  root <- getCurrentDirectory
  context <- newContext
  store <- newStore context "unix:///nix/var/nix/daemon-socket/socket" []
  evalState <- newEvalState context [] store
  flip runPkgs (Env root context store evalState "x86_64-linux") $ do
    helloC <- copyPath "./bootstrap/hello.c"

    helloDrv <- mkTccStage0 "hello" "bin/hello" (helloC :| [])
    liftIO $ print helloDrv

    helloOut <- realiseDerivation' helloDrv
    liftIO $ print helloOut
