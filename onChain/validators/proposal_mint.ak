use aiken/builtin
use aiken/dict
use aiken/bytearray
use aiken/list
use aiken/string
use aiken/transaction.{ScriptContext, Transaction, Input, Output, InlineDatum, ScriptPurpose, Mint}
use aiken/transaction/value
use aiken/transaction/credential 
use unlearn

type Base {
  name: ByteArray,
  count: Int
}

validator(base_pid: ByteArray, base_name: ByteArray) {
  fn prop_mint(_redeemer, ctx: ScriptContext) -> Bool {
    expect ScriptContext { transaction: txInfo, purpose: Mint(own_policy) } = ctx
    let Transaction {inputs, outputs, mint, ..} = txInfo
    let mint = value.from_minted_value(mint)
    
    expect Some(input) = list.find(inputs, fn(input) -> Bool { 
      expect Some(ass) = input.output.value
      |> value.flatten(_)
      |> list.find(_, fn(ass: (value.PolicyId, value.AssetName, Int)) -> Bool {
        let (input_pid, input_name, input_q) = ass

        input_pid == base_pid && input_name == base_name && input_q == 1
      }) 
       
      True
    })

    let Output { address: base_address, value: in_value, ..} = input.output

    expect [own_output] = list.filter(outputs, fn(output: Output) { 
      output.address == base_address
    })

    let Output { datum: in_datum, ..} = input.output
    let Output { datum: out_datum, value: out_value, .. } = own_output

    expect InlineDatum(base_datum_b4) = in_datum
    expect InlineDatum(base_datum_after) = out_datum

    expect Base { name: name_b4, count: count_b4 }: Base = base_datum_b4
    expect Base { name: name_after, count: count_after }: Base = base_datum_after


    name_b4 == name_after && count_b4 == count_after + 1 && unlearn.named_correct(value.flatten(mint), bytearray.concat(name_b4, bytearray.from_string(string.from_int(count_b4))), own_policy)

  }
}
