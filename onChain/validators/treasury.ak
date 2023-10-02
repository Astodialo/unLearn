use aiken/list
use aiken/transaction.{ScriptContext, Input, Output, InlineDatum }
use aiken/transaction/value
use unlearn

type Metadata {
  name:     ByteArray,
  proposal: ByteArray,
  results:  ByteArray,
  state:    ByteArray,
  amount:   Int
}

validator {
    fn treasury(_datum, _redeemer, ctx: ScriptContext) -> Bool {
        let txInfo = ctx.transaction
        let txOut = txInfo.outputs
        
        let txMint = txInfo.mint 
          |> value.from_minted_value
          |> value.flatten
        
        expect [burn_ass] = list.filter(txMint, fn(ass: (value.PolicyId, ByteArray, Int)) -> Bool {
            let (policy, _, amt) = ass

            policy == "" && amt == -1
        })

        let ref_input_option = txInfo.reference_inputs
          |> list.map(_, fn(input: Input) {
            let Input {output, ..} = input

            output
          })
          |> list.filter(_, fn(output: Output) -> Bool {
            let Output {value, ..}  = output

            unlearn.valEq(burn_ass, value.flatten(value)) 
          })
          |> list.head(_)

        expect Some(ref_input) = ref_input_option

        expect InlineDatum(ref_datum) = ref_input.datum 

        expect Metadata {
          amount:ref_amount, ..
        }: Metadata = ref_datum

        unlearn.correct_amount(txOut, ref_amount) 

      }

  }
