{-# LANGUAGE NoImplicitPrelude #-}

module Utils.OnChain where

 import qualified Plutus.V2.Ledger.Api as PlutusV2
 import Plutus.V2.Ledger.Contexts (TxInfo, TxOut, findDatum)
 import qualified PlutusTx
 import PlutusTx.IsData.Class (UnsafeFromData)
 import PlutusTx.Prelude

 --- Thanks MuesliSwap
 {-# INLINEABLE mustFindScriptDatum #-}
 mustFindScriptDatum :: (UnsafeFromData d) => TxOut -> TxInfo -> d
 mustFindScriptDatum o info = case PlutusV2.txOutDatum o of
  PlutusV2.OutputDatum (PlutusV2.Datum dat) -> PlutusTx.unsafeFromBuiltinData dat
  PlutusV2.OutputDatumHash dh -> case findDatum dh info of
    Just (PlutusV2.Datum dat) -> PlutusTx.unsafeFromBuiltinData dat
    _ -> error ()
  _ -> error ()
