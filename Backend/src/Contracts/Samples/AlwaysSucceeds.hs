{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Contracts.Samples.AlwaysSucceeds where

 import Codec.Serialise (serialise)
 import qualified Data.ByteString.Lazy as BSL
 import qualified Data.ByteString.Short as BSS
 import qualified PlutusTx
 import PlutusTx.Prelude
 import qualified Plutus.V1.Ledger.Scripts as Scripts
 import Plutus.V2.Ledger.Api (Credential(..) )
 import Cardano.Api
 import Plutus.V1.Ledger.Api (Address(..))
 import Cardano.Api.Shelley (PlutusScript(..))
 
 import Utils.Helpers (validatorHash)

 {-# INLINABLE mkAlwaysSuccedsValidator #-}
 mkAlwaysSuccedsValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
 mkAlwaysSuccedsValidator _ _ _ = ()

 alwaysSucceeds :: Scripts.Validator
 alwaysSucceeds = Scripts.mkValidatorScript $$(PlutusTx.compile [||mkAlwaysSuccedsValidator||])

 alwaysSucceedsValHash :: Scripts.ValidatorHash
 alwaysSucceedsValHash = validatorHash alwaysSucceeds

 alwaysSucceedsAddress :: Plutus.V1.Ledger.Api.Address
 alwaysSucceedsAddress = Address (ScriptCredential alwaysSucceedsValHash) Nothing

 alwaysSucceedsSerialized :: PlutusScript PlutusScriptV2
 alwaysSucceedsSerialized = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise $ alwaysSucceeds
