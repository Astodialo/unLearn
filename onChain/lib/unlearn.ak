use aiken/dict
use aiken/bytearray
use aiken/list
use aiken/transaction.{Input, Output, OutputReference}
use aiken/transaction/value

pub const master_token_name: ByteArray = "unArxh"  

pub fn valEq(ending: ByteArray, ass: (value.PolicyId, value.AssetName, Int), val: List<(value.PolicyId, value.AssetName, Int)>) -> Bool {
  list.any(val, fn(ref_ass: (value.PolicyId, value.AssetName, Int)) { 
    let (policy, name, _) = ass
    let (ref_policy, ref_name, _) = ref_ass

    let equality = ref_name
    |> bytearray.concat(_, ending)
    |> bytearray.compare(_, name) 

    equality == Equal && (policy == ref_policy)
    }
  )
}

pub fn correct_amount(outputs: List<Output>, ref_amount: Int) -> Bool {
  expect Some(output) = list.head(outputs)

  let Output {value: val, ..} = output

  value.lovelace_of(val) == ref_amount //&&
  && list.length(value.flatten(value.without_lovelace(val))) == 0 
}

pub fn own_validator_input_utxo(inputs: List<Input>, output_reference: OutputReference,) -> Output {
  expect Some(input) = list.find(inputs, fn(input) { 
    input.output_reference == output_reference
  })

  input.output
}

pub fn named_correct(values: List<(value.PolicyId, value.AssetName, Int)>, name: ByteArray, own_policy: ByteArray ) -> Bool {
  list.length(values) == 3 && list.all(values, fn(value) -> Bool {
    let (pid, asset, amt) = value

    pid == own_policy && ((asset == bytearray.concat(name, "_R")) || (asset == bytearray.concat(name, "_Claim")) || (asset == name) ) && amt == 1 
  })
}
        
pub fn value_contains_master_token(val: value.Value, own_policy: value.PolicyId) -> Bool {
  expect [(token_name, quantity)] =
    val
      |> value.tokens(own_policy)
      |> dict.to_list()

  token_name == master_token_name && quantity == 1
}

pub fn value_has_only_master_and_lovelace(val: value.Value, own_policy: value.PolicyId) -> Bool {
  expect [(policy1, amount1), (policy2, amount2)] = val
    |> value.to_dict
    |> dict.to_list()

  if policy1 == value.ada_policy_id {
    expect [(token_name, quantity)] = amount2 |> dict.to_list()

    policy2 == own_policy && quantity == 1 && token_name == master_token_name
  } else {
    expect [(token_name, quantity)] = amount1 |> dict.to_list()

    policy1 == own_policy && quantity == 1 && token_name == master_token_name
  }
}
