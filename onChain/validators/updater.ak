use aiken/builtin
use aiken/dict
use aiken/bytearray
use aiken/list
use aiken/transaction.{ScriptContext, Transaction, Input, Output, InlineDatum, ScriptPurpose, Spend}
use aiken/transaction/value
use aiken/transaction/credential
use unlearn

type Metadata {
  name:     ByteArray,
  proposal: ByteArray,
  answers:  ByteArray,
  results:  ByteArray,
  state:    ByteArray,
  amount:   Int
}

validator {
    fn updater(_datum, ctx: ScriptContext) -> Bool {
      let txInfo = ctx.transaction
      let txOut = txInfo.outputs
      let txMint = txInfo.mint 
          |> value.from_minted_value
          |> value.flatten
        
      expect Spend(own_reference) = ctx.purpose

      let own_input = unlearn.own_validator_input_utxo(txInfo.inputs, own_reference)

      let Output { address: in_address, value: in_value, ..} = own_input

      expect [own_output] = list.filter(txOut, fn(output: Output) { 
        output.address == in_address
      })

      let Output { datum: out_datum, value: out_value, .. } = own_output

      expect [burn_ass] = list.filter(txMint, fn(ass: (value.PolicyId, _, Int)) -> Bool {
        let (policy, _, amt) = ass

        policy == "" && amt == -1
      })

      unlearn.valEq(burn_ass, value.flatten(out_value))
    }
  }
