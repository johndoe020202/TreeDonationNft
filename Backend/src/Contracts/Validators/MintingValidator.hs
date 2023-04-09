{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}

module Contracts.Validators.MintingValidator where
 import Codec.Serialise (serialise)
 import qualified Data.ByteString.Lazy as BSL
 import qualified Data.ByteString.Short as BSS
 import Prelude (Show)
 import PlutusTx
 import PlutusTx.Prelude hiding (Semigroup(..), unless)
 import qualified Plutus.V1.Ledger.Scripts as Scripts
 import Plutus.V2.Ledger.Api ( Credential(..) )
 import Cardano.Api
 import Plutus.V1.Ledger.Api (Address(..), PubKeyHash)
 import Plutus.V2.Ledger.Contexts (ScriptContext, TxInfo, txSignedBy, scriptContextTxInfo)
 import Cardano.Api.Shelley (PlutusScript(..))
 
 import Utils.Helpers (validatorHash, wrap)


 data MintingDatum = MintingDatum
    { donor :: PubKeyHash
    , amount  :: Integer
    } deriving Show 

 PlutusTx.unstableMakeIsData ''MintingDatum
 
 {-# INLINABLE mkMintingValidator #-}
 mkMintingValidator :: MintingDatum -> () -> ScriptContext -> Bool
 mkMintingValidator datum () ctx = traceIfFalse "donor's signature missing" signedBydonor &&
                                   traceIfFalse "wrong amount sent, minimum must be 20 ADA" checkIfCorrectDonatedAmount 
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByDonor :: Bool
    signedByDonor = txSignedBy info $ donor datum

    txInPayments :: [Value]
    txInPayments = map txOutValue $ txInfoInputs info
    
    checkIfCorrectDonatedAmount :: Bool
    checkIfCorrectDonatedAmount = (txInPayments >= [Ada.lovelaceValueOf 20])


 mintingValidatorWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
 mintingValidatorWrapped = wrap mkMintingValidator

 mintingValidator :: Scripts.Validator
 mintingValidator = Scripts.mkValidatorScript $$(PlutusTx.compile [||mintingValidatorWrapped||])

 mintingValidatorHash :: Scripts.ValidatorHash
 mintingValidatorHash = validatorHash mintingValidator

 mintingValidatorAddress :: Plutus.V1.Ledger.Api.Address
 mintingValidatorAddress = Address (ScriptCredential mintingValidatorHash) Nothing

 mintingValidatorSerialized :: PlutusScript PlutusScriptV2
 mintingValidatorSerialized = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise $ mintingValidator  