{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Contracts.AlwaysFails where
 import Codec.Serialise (serialise)
 import qualified Data.ByteString.Lazy as BSL
 import qualified Data.ByteString.Short as BSS
 import qualified PlutusTx
 import PlutusTx.Prelude
 import qualified Plutus.V1.Ledger.Scripts as Scripts
 import Plutus.V2.Ledger.Api ( Credential(..) )
 import Cardano.Api
 import Plutus.V1.Ledger.Api (Address(..))
 import Cardano.Api.Shelley (PlutusScript(..))
 
 import Utils.Helpers (validatorHash)

 {-# INLINABLE mkAlwaysFailsValidator #-}
 mkAlwaysFailsValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
 mkAlwaysFailsValidator _ _ _ = error ()


 alwaysFails :: Scripts.Validator
 alwaysFails = Scripts.mkValidatorScript $$(PlutusTx.compile [||mkAlwaysFailsValidator||])

 alwaysFailsValHash :: Scripts.ValidatorHash
 alwaysFailsValHash = validatorHash alwaysFails

 alwaysFailsAddress :: Plutus.V1.Ledger.Api.Address
 alwaysFailsAddress = Address (ScriptCredential alwaysFailsValHash) Nothing

 alwaysFailsSerialized :: PlutusScript PlutusScriptV2
 alwaysFailsSerialized = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise $ alwaysFails