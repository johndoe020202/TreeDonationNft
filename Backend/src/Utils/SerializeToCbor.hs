import qualified PlutusTx
import PlutusTx.Builtins (serialiseToCBOR)
import Ledger.Typed.Scripts (validatorScriptHash)

policyScript :: PlutusTx.CompiledCode
policyScript = $$(PlutusTx.compile [|| mkPolicy ||])

policyScriptHash :: PlutusTx.BuiltinByteString
policyScriptHash = validatorScriptHash $ PlutusTx.getPlc policyScript

policyScriptCBOR :: PlutusTx.BuiltinByteString
policyScriptCBOR = serialiseToCBOR policyScript