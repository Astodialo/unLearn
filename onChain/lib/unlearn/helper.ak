use aiken/builtin
use aiken/dict
use aiken/bytearray
use aiken/list
use aiken/transaction.{ScriptContext, Transaction, Input, Output, InlineDatum}
use aiken/transaction/value
use aiken/transaction/credential

pub fn valEq(ass: (value.PolicyId, value.AssetName, Int), val: List<(value.PolicyId, value.AssetName, Int)>) -> Bool {
  list.any(val, fn(ref_ass: (value.PolicyId, value.AssetName, _)) { 
    let (policy, name, _) = ass
    let (ref_policy, ref_name, _) = ref_ass

    let equality = ref_name
    |> bytearray.concat(_, "_Claim")
    |> bytearray.compare(_, name) 

    equality == Equal && (policy == ref_policy)
    }
  )
}

pub fn correct_amount(outputs: List<Output>, ref_amount: Int) -> Bool {
  expect Some(output) = list.head(outputs)

  let Output {address: addr, value: val, ..} = output

  value.lovelace_of(val) == ref_amount &&
  list.length(value.flatten(value.without_lovelace(val))) == 0 
}
