use aiken/list
use aiken/transaction.{Input, Output, InlineDatum, OutputReference, TransactionId}
use assist/addresses
use aiken/transaction/value
use unlearn

type State {
  count: Int
}
pub fn test_input() -> Input {
  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: TransactionId { hash: #"acab" },
        output_index: 0,
      },
      output: Output {
        address: addresses.create_address(#"acab", #""),
        value: value.from_asset(#"acab", "beef", 40),
        datum: InlineDatum(State {count: 0} ),
        reference_script: None,
      },
    }
  input
}
pub fn test_output() -> Output {
  let output =
    Output {
      address: addresses.create_address(#"acab", #""),
      value: value.from_asset(#"acab", "beef", 40),
      datum: InlineDatum(State {count: 1}),
      reference_script: None,
    }
  output
}

pub fn test_output_R() -> Output {
  let output =
    Output {
      address: addresses.create_address(#"acab", #""),
      value: value.from_asset(#"acab", "beef_R", 40),
      datum: InlineDatum(State {count: 1}),
      reference_script: None,
    }
  output
}

test find_input(){
  list.any([test_input()], fn(input) {
    let q = value.quantity_of(input.output.value, #"acab", "beef",)

    q == 40
  })
}


test equal_dtms(){
  expect Some(unArxh_input) = list.find([test_input()], fn(input) {
    let q = value.quantity_of(input.output.value, #"acab", "beef",)

    q == 40
  })


  expect Some(unArxh_output) = list.find([test_output()], fn(output) {
    let q = value.quantity_of(output.value, #"acab", "beef",)

    q == 40 
  })
  
  expect InlineDatum(dtm) = unArxh_input.output.datum
  expect InlineDatum(out_dtm) = unArxh_output.datum

  expect State { count: count_b4 }: State = dtm
  expect State { count: count_after }: State = out_dtm


  count_b4 == count_after - 1 
}

test burn_ass(){
  let output = test_output() 
  let val = output.value
  list.any(value.flatten(val), fn(ass: (value.PolicyId, ByteArray, Int)) -> Bool {
    let (policy, _, amt) = ass

    policy == #"acab" && amt == 40 
  })
}

test valeq(){
   let out = test_output() 
   let out_R = test_output_R()
        
   expect [burn_ass] = list.filter(value.flatten(out_R.value), fn(ass: (value.PolicyId, ByteArray, Int)) -> Bool {

     let (policy, _, amt) = ass

     policy == #"acab" && amt == 40
    })   

    unlearn.valEq(burn_ass, value.flatten(out.value) )
}
