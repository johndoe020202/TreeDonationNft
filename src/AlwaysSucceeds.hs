{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DerivingStrategies  #-}

module AlwaysSucceeds where
 
 import           Codec.Serialise (serialise)
 import qualified Data.ByteString.Lazy as BSL
 import qualified Data.ByteString.Short as BSS
 
 import qualified PlutusTx
 import           PlutusTx.Prelude
 --import           Ledger hiding (mint, singleton)
 --import qualified Ledger.Typed.Scripts as Scripts
 --import           Ledger.Value as Value
 import qualified Plutus.V1.Ledger.Scripts as Scripts
 import           Plutus.V2.Ledger.Api (ScriptContext)
 import           Cardano.Api
 import           Plutus.V1.Ledger.Api (POSIXTime, Address(..))
 import qualified Plutus.V2.Ledger.Contexts as Contexts
 import qualified Plutus.V2.Ledger.Api as PlutusV2
 import           Plutus.V2.Ledger.Api (Credential(..))
 import           Plutus.V1.Ledger.Interval
 import           Cardano.Api.Shelley (PlutusScript(..))
 import           Plutus.V2.Ledger.Api (TxInfo)
 import           Plutus.V2.Ledger.Contexts (findDatum)

 import           Shared (validatorHash, wrap)

 {-# INLINABLE mkAlwaysSuccedsValidator #-}
 mkAlwaysSuccedsValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
 mkAlwaysSuccedsValidator _ _ _ = ()

 alwaysSucceeds :: Scripts.Validator
 alwaysSucceeds = Scripts.mkValidatorScript $$(PlutusTx.compile [||mkAlwaysSuccedsValidator||])                 

 alwaysSucceedsValHash :: Scripts.ValidatorHash
 alwaysSucceedsValHash = Shared.validatorHash alwaysSucceeds                      

 alwaysSucceedsAddress :: Plutus.V1.Ledger.Api.Address
 alwaysSucceedsAddress = Address (ScriptCredential alwaysSucceedsValHash) Nothing               

 alwaysSucceedsSerialized :: PlutusScript PlutusScriptV1
 alwaysSucceedsSerialized = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise $ alwaysSucceeds 
