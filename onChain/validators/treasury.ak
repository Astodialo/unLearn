use aiken/builtin
use aiken/dict
use aiken/bytearray
use aiken/list
use aiken/transaction.{ScriptContext, Transaction, Input, Output}
use aiken/transaction/value
use unlearn/helper

validator {
    fn treasury(_datum, _redeemer, ctx: ScriptContext) -> Bool {
        let txInfo = ctx.transaction
        
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

        let ref_input = txInfo.reference_inputs
          |> list.map(_, fn(input: Input) {
            let Input {output_reference, output} = input

            output
          })
          |> list.map(_, fn(output: Output) -> Bool {
            let Output {address, value, ..}  = output

          helper.valEq(burn_ass, value.flatten(value)) 
            }
          )
          |> list.head(_)

        todo

      }

  }
