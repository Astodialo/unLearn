use aiken/bytearray
use aiken/cbor
use aiken/list
use aiken/string
use aiken/transaction.{ScriptContext, Transaction, Input, Output, InlineDatum, Mint, Spend, OutputReference, find_input}
use aiken/transaction/value
use aiken/transaction/credential
use unlearn

type State {
  count: Int,
}

type MintingAction {
  Genesis
  Mintin
  Update
  Withdraw
}

type Metadata {
  name: ByteArray,
  proposal: ByteArray,
  results: ByteArray,
  state: ByteArray,
  amount: Int,
}

validator(utxo_ref: OutputReference) {
  fn prop_mint(redeemer: MintingAction, ctx: ScriptContext) -> Bool {
    trace "ho"
    
    expect ScriptContext { transaction: txInfo, purpose: Mint(own_policy) } = ctx
    let Transaction {inputs, outputs, mint, ..} = txInfo
    let mint = value.from_minted_value(mint)

    when redeemer is {
      Genesis -> {
        expect list.any(inputs, fn(input) { input.output_reference == utxo_ref })?

        expect [Output {datum: InlineDatum(output_datum), value, .. }] = list.filter(outputs, fn(output) {
          output.address.payment_credential == credential.ScriptCredential(own_policy)
          && value.quantity_of(output.value, own_policy, unlearn.master_token_name) == 1
        })

        expect unlearn.value_contains_master_token(mint, own_policy)

        expect unlearn.value_has_only_master_and_lovelace(value, own_policy)

        expect output_state: State = output_datum

        let initial_state =  State {count: 0}

        (initial_state == output_state)?
      }
      Mintin -> {
        trace "hi"
        
        expect Some(unArxh_input) = list.find(inputs, fn(input) {
          let q = value.quantity_of(input.output.value, own_policy, unlearn.master_token_name,)

          if (q == 1) {
            trace "there is input"
            True
          } else {
            trace "there is no input"
            False
          }

          //q == 1 
        })

        expect Some(unArxh_output) = list.find(outputs, fn(output: Output) { 
          let q = value.quantity_of(output.value, own_policy, unlearn.master_token_name,)

          if (q == 1) {
            trace "there is output"
            True
          }else {
            trace "there is no output"
            False
          }

          //q == 1
        })

        expect InlineDatum(in_dtm) = unArxh_input.output.datum 
        expect InlineDatum(out_dtm) = unArxh_output.datum

        expect State { count: count_b4 }: State = in_dtm 
        expect State { count: count_after }: State = out_dtm 

        (count_b4 == count_after - 1)? 
        && (unlearn.named_correct(value.flatten(mint), bytearray.concat("proposal_", bytearray.from_string(string.from_int(count_b4))), own_policy))? 

        //expect [updater_output] =  list.filter(outputs, fn(output: Output) {
        //  output.address == updater_addr
        //})
        //let Output { datum: update_datum, value: out_value, .. } = updater_output
        //expect InlineDatum(updt_datum) = update_datum
       
        //expect Metadata { name, proposal, results, state, amount }: Metadata = updt_datum

        //name == bytearray.concat(name_b4, bytearray.from_string(string.from_int(count_b4))) &&
        //results == "" &&
        //state == "INIT" &&
        //amount == 0
      }
      Update -> {
        trace "hi"
        expect [burn_ass] = list.filter(value.flatten(mint), fn(ass: (value.PolicyId, ByteArray, Int)) -> Bool {
        let (policy, _, amt) = ass

        policy == own_policy && amt == -1
      })

        expect [own_output] = list.filter(outputs, fn(output) -> Bool {
          output.address.payment_credential == credential.ScriptCredential(own_policy)
        })

        let Output {value: out_value, ..} = own_output

        unlearn.valEq("_R", burn_ass, value.flatten(out_value))
      }
      Withdraw -> {
        trace "hi"
        expect [burn_ass] = list.filter(value.flatten(mint), fn(ass: (value.PolicyId, ByteArray, Int)) -> Bool {
          let (policy, _, amt) = ass

          policy == own_policy && amt == -1
        })

        let ref_input = txInfo.reference_inputs
          |> list.map(_, fn(input: Input) {
            let Input {output, ..} = input

            output
          })
          |> list.filter(_, fn(output: Output) -> Bool {
            let Output {value, ..} = output

            unlearn.valEq("_Claim", burn_ass, value.flatten(value))
          })

        expect [ref_input] = ref_input

        expect InlineDatum(ref_datum) = ref_input.datum
        
        expect Metadata {amount, ..}: Metadata = ref_datum
              
        expect [burn_input_output] = inputs 
          |> list.map(_, fn(input: Input) {
            let Input {output, ..} = input

            output
          })
          |> list.filter(_, fn(output: Output) -> Bool {
            let Output {value, ..} = output
            let flat_val = value.flatten(value)
            list.any(flat_val, fn(ass: (value.PolicyId, value.AssetName, Int)) {
              let (policy, name, amt) = ass
              let (burn_policy, burn_name, burn_amt) =burn_ass

              if (policy == own_policy &&  name == burn_name) {
                trace "assinput"
                trace cbor.diagnostic(amt)
                True
              }else {False}
            }) 
          })

        let val_from_outputs = 0
        let user_lvlc_input = 0

        let user_lvlc_input = list.foldl(inputs, user_lvlc_input, fn(input: Input, counter: Int) -> Int {
          trace "beep"
          if (input.output.address.payment_credential == burn_input_output.address.payment_credential) {
            trace "initiator input" 
            trace cbor.diagnostic(input.output.value)
            user_lvlc_input + value.lovelace_of(input.output.value)
            
          }else {
            user_lvlc_input 
          }
        })


        let val_from_outputs = list.foldl(outputs, val_from_outputs, fn(output: Output, counter: Int) -> Int {
          let Output {address, value, ..} = output

          trace "yeet"
          if (address.payment_credential == burn_input_output.address.payment_credential) {
            trace "initiator output" 
            trace cbor.diagnostic(value.lovelace_of(value))
            val_from_outputs + value.lovelace_of(value)
          }else {
            val_from_outputs
          }
        })
        
        trace "ho"
        trace cbor.diagnostic(user_lvlc_input)
        trace cbor.diagnostic(val_from_outputs)
        trace "ho"
        //let Output {value, ..} = output
        (val_from_outputs == user_lvlc_input + amount)? 
        //&& (list.length(value.flatten(value.without_lovelace(value))) == 0)? 
      }
    }
  }
  fn spend(datum, redeemer: MintingAction, ctx: ScriptContext) -> Bool {
    expect ScriptContext { transaction: txInfo, purpose: Spend(own_reference) } = ctx
    let Transaction {inputs, outputs, mint, ..} = txInfo
    let mint = value.from_minted_value(mint)

    trace "hi"
    expect Some(own_input) = inputs |> find_input(own_reference)

    let Input {output: Output { address: credential.Address { payment_credential, .. }, .. }, ..} = own_input

    expect credential.ScriptCredential(own_policy) = payment_credential 

    when redeemer is { 
      Genesis -> { False }
      Mintin -> {
        trace "hi"

        expect Some(unArxh_output) = list.find(outputs, fn(output: Output) { 
          let q = value.quantity_of(output.value, own_policy, unlearn.master_token_name,)

          if (q == 1) {
            trace "there is output"
            True
          }else {
            trace "there is no output"
            False
          }

          //q == 1
        })

        expect InlineDatum(out_dtm) = unArxh_output.datum

        expect State { count: count_b4 }: State = datum 
        expect State { count: count_after }: State = out_dtm 

        (count_b4 == count_after - 1)? 
        //&& unlearn.named_correct(value.flatten(mint), bytearray.concat("proposal_", bytearray.from_string(string.from_int(count_b4))), own_policy) 

        //expect [updater_output] =  list.filter(outputs, fn(output: Output) {
        //  output.address == updater_addr
        //})
        //let Output { datum: update_datum, value: out_value, .. } = updater_output
        //expect InlineDatum(updt_datum) = update_datum
        
        //expect Metadata { name, proposal, results, state, amount }: Metadata = updt_datum

        //name == bytearray.concat(name_b4, bytearray.from_string(string.from_int(count_b4))) &&
        //results == "" &&
        //state == "INIT" &&
        //amount == 0
      }
      Update -> {
        trace "hi"
        expect [burn_ass] = list.filter(value.flatten(mint), fn(ass: (value.PolicyId, ByteArray, Int)) -> Bool {
          let (policy, _, amt) = ass

          policy == own_policy && amt == -1
      })

        expect [own_output] = list.filter(outputs, fn(output) -> Bool {
          output.address.payment_credential == credential.ScriptCredential(own_policy)
        })

        let Output {value: out_value, ..} = own_output

        (unlearn.valEq("_R", burn_ass, value.flatten(out_value)))?
      }
      Withdraw -> {
        expect [burn_ass] = list.filter(value.flatten(mint), fn(ass: (value.PolicyId, ByteArray, Int)) -> Bool {
          let (policy, _, amt) = ass

          if (policy == own_policy && amt == -1) {
            trace "ass"
            True
          }else {False}
        })

        let ref_input = txInfo.reference_inputs
          |> list.map(_, fn(input: Input) {
            let Input {output, ..} = input

            output
          })
          |> list.filter(_, fn(output: Output) -> Bool {
            let Output {value, ..} = output

            if (unlearn.valEq("_Claim", burn_ass, value.flatten(value))) {
              trace "ref_input"
              True
            }else {False}
          })

        expect [burn_input_output] = inputs 
          |> list.map(_, fn(input: Input) {
            let Input {output, ..} = input

            output
          })
          |> list.filter(_, fn(output: Output) -> Bool {
            let Output {value, ..} = output
            let flat_val = value.flatten(value)
            list.any(flat_val, fn(ass: (value.PolicyId, value.AssetName, Int)) {
              let (policy, name, amt) = ass
              let (burn_policy, burn_name, _) = burn_ass

              if (policy == own_policy &&  name == burn_name) {
                trace "assinput"
                trace cbor.diagnostic(amt)
                True
              }else {False}
            }) 
          })

        expect [ref_input] = ref_input

        expect InlineDatum(ref_datum) = ref_input.datum

        trace cbor.diagnostic(ref_datum)
        
        expect Metadata {amount, ..}: Metadata = ref_datum
              
        trace cbor.diagnostic(amount)
        
        let val_from_outputs = 0
        let user_lvlc_input = 0

        let  user_lvlc_input = list.foldl(inputs, user_lvlc_input, fn(input: Input, counter: Int) -> Int {
          trace "beep"
          if (input.output.address.payment_credential == burn_input_output.address.payment_credential) {
            trace "initiator input" 
            user_lvlc_input + value.lovelace_of(input.output.value)
            
          }else {
            user_lvlc_input 
          }
        })


        let val_from_outputs = list.foldl(outputs, val_from_outputs, fn(output: Output, counter: Int) -> Int {
          let Output {address, value, ..} = output

          trace "yeet"
          if (address.payment_credential == burn_input_output.address.payment_credential) {
            trace "initiator output" 
            trace cbor.diagnostic(value.lovelace_of(value))
            val_from_outputs + value.lovelace_of(value)
          }else {
            val_from_outputs
          }
        })
        
        trace "hi"
        trace cbor.diagnostic(user_lvlc_input + amount)
        trace cbor.diagnostic(val_from_outputs)
        trace "hi"
        //let Output {value, ..} = output
        (val_from_outputs == user_lvlc_input + amount)? 
        //&& (list.length(value.flatten(value.without_lovelace(value))) == 0)? 
      }
    }
  }
}
