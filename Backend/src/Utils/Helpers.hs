{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Helpers (wrap, validatorHash) where

import qualified Cardano.Api.Shelley as Shelley
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.ByteString.Short as BSShort
import qualified Plutus.V1.Ledger.Scripts as Scripts
import PlutusTx
import PlutusTx.Prelude

wrap ::
  forall a b c.
  ( UnsafeFromData a,
    UnsafeFromData b,
    UnsafeFromData c
  ) =>
  (a -> b -> c -> Bool) ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
wrap f a b c =
  check
    ( f
        (unsafeFromBuiltinData a)
        (unsafeFromBuiltinData b)
        (unsafeFromBuiltinData c)
    )

validatorHash :: Scripts.Validator -> Scripts.ValidatorHash
validatorHash = Scripts.ValidatorHash . Scripts.getScriptHash . scriptHash . Scripts.getValidator

scriptHash :: Scripts.Script -> Scripts.ScriptHash
scriptHash =  Scripts.ScriptHash . toBuiltin . Shelley.serialiseToRawBytes . Shelley.hashScript . toCardanoApiScript

toCardanoApiScript :: Scripts.Script -> Shelley.Script Shelley.PlutusScriptV2
toCardanoApiScript = Shelley.PlutusScript Shelley.PlutusScriptV2 . Shelley.PlutusScriptSerialised . BSShort.toShort . BSLazy.toStrict . serialise