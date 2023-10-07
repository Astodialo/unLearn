use aiken/list
use aiken/transaction.{Input, Output, InlineDatum, OutputReference, TransactionId}
use assist/addresses
use aiken/transaction/value
use unlearn

type State {
  count: Int
}

type Metadata {
  name:     ByteArray,
  proposal: ByteArray,
  results:  ByteArray,
  state:    ByteArray,
  amount:   Int
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

pub fn test_input2() -> Input {
  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: TransactionId { hash: #"acab" },
        output_index: 0,
      },
      output: Output {
        address: addresses.create_address(#"acab", #""),
        value: value.from_asset(#"acab", "beef", 40),
        datum: InlineDatum(Metadata {
          name:     "",
          proposal: "",
          results:  "",
          state:    "",
          amount:   100
        }),
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

pub fn test_output_lvlc() -> Output {
  let output =
    Output {
      address: addresses.create_address(#"acab", #""),
      value: value.from_lovelace(100),
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

  unlearn.valEq("_R", burn_ass, value.flatten(out.value) )
}

test correct_amt(){
  let out = test_output_lvlc()  

  expect Some(output) = list.head([out])

  let Output {value: val, ..} = output

  value.lovelace_of(val) == 100
}

!test no_output(){
  expect [output] = [] 

  let Output {value: val, ..} = output

  value.lovelace_of(val) == 100 
  //&& list.length(outputs) == 1
}


test find_ref_input(){
  let out_R = test_output_R()
  let out_lvlc = test_output_lvlc()
      
  expect [burn_ass] = list.filter(value.flatten(out_R.value), fn(ass: (value.PolicyId, ByteArray, Int)) -> Bool {

    let (policy, _, amt) = ass

    policy == #"acab" && amt == 40
  })   

  let ref_input = [test_input2()]
    |> list.map(_, fn(input: Input) {
      let Input {output, ..} = input

      output
    })
    |> list.filter(_, fn(output: Output) -> Bool {
      let Output {value, ..} = output

      unlearn.valEq("_R", burn_ass, value.flatten(value))
    })

  expect [ref_input] = ref_input

  expect InlineDatum(ref_datum) = ref_input.datum

  expect Metadata {amount, ..}: Metadata = ref_datum

  let Output {value: val, ..} = out_lvlc

  value.lovelace_of(val) == amount 
  && list.length(value.flatten(value.without_lovelace(val))) == 0 
}

test lvlc_from_sender_utxo(){
  let val_from_outputs = 0

  let outpt = test_output_lvlc()

  let val_from_outputs = list.foldr([outpt], val_from_outputs, fn(output: Output, total: Int) -> Int {
    let Output {address, value, ..} = output

    trace "yeet"
    if (address.payment_credential == output.address.payment_credential){//credential.ScriptCredential(own_policy)) {
      trace "yoot" 
      val_from_outputs + value.lovelace_of(value)
    }else {
      val_from_outputs
    }
  })
  
  val_from_outputs == 100
}

