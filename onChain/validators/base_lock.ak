use aiken/builtin
use aiken/dict
use aiken/bytearray
use aiken/list
use aiken/string
use aiken/transaction.{ScriptContext, Transaction, Input, Output, InlineDatum, ScriptPurpose, Spend}
use aiken/transaction/value
use aiken/transaction/credential 
use unlearn

type Base {
  name: ByteArray,
  count: Int
}

validator() {
  fn lock_base(_datum, ctx: ScriptContext) -> Bool{
    let txInfo = ctx.transaction
    let txOut = txInfo.outputs
    let txMint = txInfo.mint 
        |> value.from_minted_value
        |> value.flatten
      
    expect Spend(own_reference) = ctx.purpose

    let own_input = unlearn.own_validator_input_utxo(txInfo.inputs, own_reference)

    let Output { address: in_address, value: in_value, datum: in_datum, ..} = own_input

    expect [own_output] = list.filter(txOut, fn(output: Output) { 
      output.address == in_address
    })

    let Output { datum: in_datum, ..} = own_input
    let Output { datum: out_datum, value: out_value, .. } = own_output

    expect InlineDatum(base_datum_b4) = in_datum
    expect InlineDatum(base_datum_after) = out_datum

    expect Base { name: name_b4, count: count_b4 }: Base = base_datum_b4
    expect Base { name: name_after, count: count_after }: Base = base_datum_after

    name_b4 == name_after && count_b4 == count_after + 1
  }
}
