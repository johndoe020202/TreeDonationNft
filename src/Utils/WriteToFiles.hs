{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.WriteToFiles (writeAlwaysSucceedsPlutusFile, writeAlwaysFailsPlutusFile, encodePlutusData) where

import qualified Cardano.Api as Api
import PlutusTx.Prelude (Either (..), Maybe (Nothing), ($), (++), (>>=))
import System.FilePath ( FilePath )
import Prelude (IO, print, putStrLn)
import Data.Aeson ( encode )
import Cardano.Api (scriptDataToJson, ScriptDataJsonSchema (..))
import Cardano.Api.Shelley (fromPlutusData)
import PlutusTx (builtinDataToData, ToData (toBuiltinData))
import qualified Data.ByteString.Lazy as BSL

import Contracts.AlwaysSucceeds
import Contracts.AlwaysFails

writeAlwaysSucceedsPlutusFile :: FilePath -> IO ()
writeAlwaysSucceedsPlutusFile filePath =
  Api.writeFileTextEnvelope filePath Nothing alwaysSucceedsSerialized >>= \case
    Left err -> print $ Api.displayError err
    Right () -> putStrLn $ "wrote validator to file " ++ filePath

writeAlwaysFailsPlutusFile :: FilePath -> IO ()
writeAlwaysFailsPlutusFile filePath =
  Api.writeFileTextEnvelope filePath Nothing alwaysFailsSerialized >>= \case
    Left err -> print $ Api.displayError err
    Right () -> putStrLn $ "wrote validator to file " ++ filePath

encodePlutusData :: forall a. ToData a => a -> BSL.ByteString
encodePlutusData a = Data.Aeson.encode $ scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (builtinDataToData $ toBuiltinData a)
