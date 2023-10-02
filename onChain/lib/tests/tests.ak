use aiken/builtin
use aiken/interval.{Interval, IntervalBound, NegativeInfinity, PositiveInfinity}
use aiken/dict
use aiken/bytearray
use aiken/list
use aiken/string
use aiken/transaction.{ScriptContext, Transaction, Input, Output, InlineDatum, ScriptPurpose, Mint, Spend, NoDatum, OutputReference, TransactionId}
use assist/addresses
use aiken/transaction/value
use aiken/transaction/credential 
use unlearn
use tests/fake_tx 

type State {
  count: Int
}
pub fn test_input2() -> Input {
  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: TransactionId { hash: #"acab" },
        output_index: 0,
      },
      output: Output {
        address: addresses.create_address(#"acab", #""),
        value: value.from_asset(#"acab", #"beef", 40),
        datum: InlineDatum(State {count: 0} ),
        reference_script: None,
      },
    }
  input
}
pub fn test_output2() -> Output {
  let output =
    Output {
      address: addresses.create_address(#"acab", #""),
      value: value.from_asset(#"acab", #"beef", 40),
      datum: InlineDatum(State {count: 1}),
      reference_script: None,
    }
  output
}

test find_input(){
  list.any(fake_tx.test_inputs(1), fn(input) {
    let q = value.quantity_of(input.output.value, #"acab", #"beef",)

    q == 100
  })
}


test equal_dtms(){
  expect Some(unArxh_input) = list.find([test_input2()], fn(input) {
    let q = value.quantity_of(input.output.value, #"acab", #"beef",)

    q == 40
  })


  expect Some(unArxh_output) = list.find([test_output2()], fn(output) {
    let q = value.quantity_of(output.value, #"acab", #"beef",)

    q == 40 
  })
  
  expect InlineDatum(dtm) = unArxh_input.output.datum
  expect InlineDatum(out_dtm) = unArxh_output.datum

  expect State { count: count_b4 }: State = dtm
  expect State { count: count_after }: State = out_dtm


  count_b4 == count_after - 1 
}

