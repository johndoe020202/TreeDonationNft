{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}

module Contracts.Policies.MintingPolicy where
 import PlutusTx
 import PlutusTx.Prelude hiding (Semigroup(..), unless)
 import qualified Plutus.V1.Ledger.Scripts as Scripts
 import Plutus.V2.Ledger.Api ( Credential(..) )
 import Cardano.Api
 import Plutus.V1.Ledger.Api (Address(..), PubKeyHash)
 import Plutus.V2.Ledger.Contexts (ScriptContext, TxInfo, txSignedBy, scriptContextTxInfo, txInInfoOutRef, txInfoMint)
 import Cardano.Api.Shelley (PlutusScript(..))
 
 data MintRedeemer = Mint | Burn 
 PlutusTx.unstableMakeIsData ''MintRedeemer

 {-# INLINABLE mkTokenPolicy #-}
 mkTokenPolicy :: TxOutRef -> MintRedeemer -> ScriptContext -> Bool
 mkTokenPolicy oref rdmr ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                               traceIfFalse "wrong amount minted" checkMintedAmount &&
                                 case rdmr of 
                                   Mint -> True 
                                   _ -> False 

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    
    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, _, amt')] -> _ == tn && amt' == 1
        _              -> False

 mkTokenPolicyWrapped :: TxOutRef -> BuiltinData -> ScriptContext -> Bool
 mkTokenPolicyWrapped oref rdmr ctx = mkTokenPolicy oref (unsafeToBuiltinData rdmr) ctx 
