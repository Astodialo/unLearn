use aiken/list
use aiken/transaction.{ScriptContext, Output, Spend}
use aiken/transaction/value
use unlearn

validator {
    fn updater(_datum, ctx: ScriptContext) -> Bool {
      let txInfo = ctx.transaction
      let txOut = txInfo.outputs
      let txMint = txInfo.mint 
          |> value.from_minted_value
          |> value.flatten
        
      expect Spend(own_reference) = ctx.purpose

      let own_input = unlearn.own_validator_input_utxo(txInfo.inputs, own_reference)

      let Output { address: in_address, ..} = own_input

      expect [own_output] = list.filter(txOut, fn(output: Output) { 
        output.address == in_address
      })

      let Output { value: out_value, .. } = own_output

      expect [burn_ass] = list.filter(txMint, fn(ass: (value.PolicyId, ByteArray, Int)) -> Bool {
        let (policy, _, amt) = ass

        policy == "" && amt == -1
      })

      unlearn.valEq(burn_ass, value.flatten(out_value))
    }
  }
