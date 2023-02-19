{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.WriteToFiles (writeAlwaysSucceedsPlutusFile, writeAlwaysFailsPlutusFile, writeMintingValidatorPlutusFile, encodePlutusData) where

import qualified Cardano.Api as Api
import PlutusTx.Prelude (Either (..), Maybe (Nothing), ($), (++), (>>=))
import System.FilePath ( FilePath )
import Prelude (IO, print, putStrLn)
import Data.Aeson ( encode )
import Cardano.Api (scriptDataToJson, ScriptDataJsonSchema (..), PlutusScriptV2)
import Cardano.Api.Shelley (fromPlutusData, PlutusScript(..))
import PlutusTx (builtinDataToData, ToData (toBuiltinData))
import qualified Data.ByteString.Lazy as BSL

import Contracts.Samples.AlwaysSucceeds
import Contracts.Samples.AlwaysFails
import Contracts.Validators.MintingValidator

writeAlwaysSucceedsPlutusFile :: FilePath -> IO ()
writeAlwaysSucceedsPlutusFile filePath = writeScriptFile filePath alwaysSucceedsSerialized

writeAlwaysFailsPlutusFile :: FilePath -> IO ()
writeAlwaysFailsPlutusFile filePath = writeScriptFile filePath alwaysFailsSerialized

writeMintingValidatorPlutusFile :: FilePath -> IO ()
writeMintingValidatorPlutusFile filePath = writeScriptFile filePath mintingValidatorSerialized

writeScriptFile :: FilePath -> PlutusScript PlutusScriptV2 -> IO ()
writeScriptFile filePath script =
  Api.writeFileTextEnvelope filePath Nothing script >>= \case
    Left err -> print $ Api.displayError err
    Right () -> putStrLn $ "wrote validator to file " ++ filePath

encodePlutusData :: forall a. ToData a => a -> BSL.ByteString
encodePlutusData a = Data.Aeson.encode $ scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (builtinDataToData $ toBuiltinData a)
