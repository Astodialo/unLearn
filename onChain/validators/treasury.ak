use aiken/builtin
use aiken/dict
use aiken/bytearray
use aiken/list
use aiken/transaction.{ScriptContext, Transaction, Input, Output, InlineDatum}
use aiken/transaction/value
use aiken/transaction/credential
use unlearn/helper

type Metadata {
  name:     ByteArray,
  proposal: ByteArray,
  answers:  ByteArray,
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
        
        let burn_ass_option = txMint
          |> list.filter(_, fn(ass: (value.PolicyId, _, Int)) -> Bool {
            let (policy, _, amt) = ass

            policy == "" && amt == -1
          })
          |> list.head

          let burn_ass = when burn_ass_option is {
            Some(ass) -> ass
            None  -> todo
          }

        let ref_input_option = txInfo.reference_inputs
          |> list.map(_, fn(input: Input) {
            let Input {output_reference, output} = input

            output
          })
          |> list.filter(_, fn(output: Output) -> Bool {
            let Output {address, value, ..}  = output

            helper.valEq(burn_ass, value.flatten(value)) 
          })
          |> list.head(_)

          let ref_input = when ref_input_option is {
            Some(in) -> in
            None  -> todo
          }

        expect InlineDatum(ref_datum) = ref_input.datum 

        expect Metadata {
          //name: ref_name,
          //proposal: ref_prop,
          //answers: ref_answers,
          //results: ref_results,
          //state: ref_state,
          amount:ref_amount, ..
        }: Metadata = ref_datum

        helper.correct_amount(txOut, ref_amount)  

      }

  }
