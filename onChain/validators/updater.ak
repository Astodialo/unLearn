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
    fn updater(_datum, ctx: ScriptContext) -> Bool {
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
         
      expect Some(burn_ass) = burn_ass_option
      
      let input_ref_option = txInfo.inputs
        |> list.map(_, fn(input: Input) {
          let Input {output_reference, output} = input

          output
        })
        |> list.filter(_, fn(output: Output) -> Bool {
          let Output {address, value, ..}  = output

          helper.valEq(burn_ass, value.flatten(value)) 
        })
        |> list.head(_)

      expect Some(input_ref_token) = input_ref_option

      let output_ref_token_option = txOut 
        |> list.filter(_,fn(output: Output) -> Bool{
          let Output {address, value, ..} = output

          helper.valEq(burn_ass, value.flatten(value))
        })
        |> list.head(_)

      expect Some(output_ref_token) = output_ref_token_option

      expect Output {address: in_addr, ..} = input_ref_token 
      expect credential.Address {payment_credential: in_cred, ..} = in_addr 
      expect credential.ScriptCredential(in_script) = in_cred

      expect Output {address: out_addr, ..} = output_ref_token
      expect credential.Address {payment_credential: out_cred, ..} = out_addr 
      expect credential.ScriptCredential(out_script) = out_cred


      list.length(txMint) == 1 &&  bytearray.compare(out_script, in_script) == Equal

    }
  }
