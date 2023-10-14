use aiken/bytearray
use aiken/cbor
use aiken/list
use aiken/string
use aiken/transaction.{ScriptContext, Transaction, Input, Output, InlineDatum, Mint, Spend, OutputReference, find_input}
use aiken/transaction/value
use aiken/transaction/credential
use unlearn

// Type of the unArxh datum
type State {
  count: Int,
}

// Type of the scripts redeemer
type MintingAction {
  Genesis
  Mintin
  Update
  Withdraw
}

// Type of the proposal-x datum
type Metadata {
  proposal: ByteArray,
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
        // Check if the utxo, that the validator is parametrized with, is an input of the transaction. After the utxo is consumed no other utxo with the same ref will exist, proving that no other unArxh token is minted. 
        expect list.any(inputs, fn(input) { input.output_reference == utxo_ref })?

        // Find in the outputs, only one output with the value of the unArxh token included.
        expect [Output {datum: InlineDatum(output_datum), value, .. }] = list.filter(outputs, fn(output) {
          output.address.payment_credential == credential.ScriptCredential(own_policy)
          && value.quantity_of(output.value, own_policy, unlearn.master_token_name) == 1
        })

        // Check that only one unArxh token is minted
        expect unlearn.value_contains_master_token(mint, own_policy)

        expect unlearn.value_has_only_master_and_lovelace(value, own_policy)

        expect output_state: State = output_datum
        
        let initial_state =  State {count: 0}

        // Check that the count field, inthe initial State in the unArxh datum, is 0.
        (initial_state == output_state)?
      }
      Mintin -> {
        trace "hi"
        
        // Find the input, that contains the unArxh token.
        expect Some(unArxh_input) = list.find(inputs, fn(input) {
          let q = value.quantity_of(input.output.value, own_policy, unlearn.master_token_name,)

          if (q == 1) {
            trace "there is input"
            True
          } else {
            trace "there is no input"
            False
          }
        })

        // Find the output, that contains the unArxh token.
        expect Some(unArxh_output) = list.find(outputs, fn(output: Output) { 
          let q = value.quantity_of(output.value, own_policy, unlearn.master_token_name,)

          if (q == 1) {
            trace "there is output"
            True
          }else {
            trace "there is no output"
            False
          }
        })


        expect InlineDatum(in_dtm) = unArxh_input.output.datum 
        expect InlineDatum(out_dtm) = unArxh_output.datum

        expect State { count: count_b4 }: State = in_dtm 
        expect State { count: count_after }: State = out_dtm 

        // Find the output, that contains the proposal token.
        expect Some(proposal_output) = list.find(outputs, fn(output: Output) {
          let q = value.quantity_of(output.value, own_policy, bytearray.concat("proposal_", bytearray.from_string(string.from_int(count_b4))))

          if (q == 1) {
            trace "proposal out"
            True
          }else {
            trace "proposal not out"
            False
          }
        }) 

        let Output { datum: proposal_datum, value: out_value, .. } = proposal_output
        expect InlineDatum(prpsl_datum) = proposal_datum
       
        expect Metadata { proposal, state, amount }: Metadata = prpsl_datum

        // Check that the state field, in the datum of the proposal datum is set to "INIT"
        state == "INIT"
        // Check that the count field, in the datum of the proposal datum, has increased
        && (count_b4 == count_after - 1)? 
        // Check that the minted tokens have the correct names
        && (unlearn.named_correct(value.flatten(mint), bytearray.concat("proposal_", bytearray.from_string(string.from_int(count_b4))), own_policy))? 
      }
      Update -> {
        trace "hi"
        // Check that there is only one burned asset with the policy of the script 
        expect [burn_ass] = list.filter(value.flatten(mint), fn(ass: (value.PolicyId, ByteArray, Int)) -> Bool {
        let (policy, _, amt) = ass

        policy == own_policy && amt == -1
      })

        // Check that there is only one input from the validator
        expect [own_input] = list.filter(inputs, fn(input) -> Bool {
          input.output.address.payment_credential == credential.ScriptCredential(own_policy)
        })

        // Check that there is only one output going back to the validator
        expect [own_output] = list.filter(outputs, fn(output) -> Bool {
          output.address.payment_credential == credential.ScriptCredential(own_policy)
        })


        let Output {value: out_value, datum: in_datum ..} = own_input.output
        let Output {value: out_value, datum: out_datum ..} = own_output

        expect InlineDatum(in_dtm) = in_datum
        expect InlineDatum(out_dtm) = out_datum

        expect Metadata { proposal: in_proposal, state: in_state, amount: in_amount }: Metadata = in_dtm
        expect Metadata { proposal: out_proposal,  state: out_state, amount: out_amount }: Metadata = out_dtm
        
        // Check that these values havent changed
        (in_proposal == out_proposal)? 
        && (in_amount == out_amount)?
        // Check that the state of the input was "INIT"
        && (in_state == "INIT")?
        // Check that the state has correctly been updated
        && (out_state == "COMPLETE" || out_state == "CANCELLED")?
        // Check that the policies and names of the burned and outputed token are the same, if not for the _R ending
        && unlearn.valEq("_R", burn_ass, value.flatten(out_value))
      }
      Withdraw -> {
        trace "hi"

        // Check that there is only one burned asset with the policy of the script 
        expect [burn_ass] = list.filter(value.flatten(mint), fn(ass: (value.PolicyId, ByteArray, Int)) -> Bool {
          let (policy, _, amt) = ass

          policy == own_policy && amt == -1
        })

        // Find the reference input that has the same policy and name with the burned asset
        let ref_input = txInfo.reference_inputs
          |> list.map(_, fn(input: Input) {
            let Input {output, ..} = input

            output
          })
          |> list.filter(_, fn(output: Output) -> Bool {
            let Output {value, ..} = output

            unlearn.valEq("_Claim", burn_ass, value.flatten(value))
          })

        // Check that the refence input is one
        expect [ref_input] = ref_input

        expect InlineDatum(ref_datum) = ref_input.datum
        
        expect Metadata {amount, state, ..}: Metadata = ref_datum
              
        // Find the input's output that contains the proposal-x-Claim that is burned. 
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

        let script_inputs = list.filter(inputs, fn(input: Input) -> Bool {
          input.output.address.payment_credential ==  credential.ScriptCredential(own_policy)
        })

        // Find the input from the address that burns the proposal-x-Claim
        expect [initiator_input] = list.filter(inputs, fn(input: Input) -> Bool {
          input.output.address.payment_credential == burn_input_output.address.payment_credential
        })

        // Find the output from the address that burns the proposal-x-Claim
        expect [initiator_output] = list.filter(outputs, fn(output: Output) -> Bool{
          output.address.payment_credential == burn_input_output.address.payment_credential
        })
        
        // Find the values in lovelace in the input and the output.
        let user_lvlc_input = value.lovelace_of(initiator_input.output.value)
        let val_from_outputs = value.lovelace_of(initiator_output.value)
        
        trace "lvlvc input"
        trace cbor.diagnostic(user_lvlc_input + amount)
        trace "lvlc output"
        trace cbor.diagnostic(val_from_outputs)
        trace cbor.diagnostic(val_from_outputs)

        // The output to the address, is equal with the amount of lovelace inputted from the address plus the amount stated on the proposal-X datum.
        (val_from_outputs == user_lvlc_input + amount)?
        // Check that the state in proposal-x's datum is "COMPLETE"
        && (state == "COMPLETE")? 
        // Check that all the inputs from the script address have the datum "Banka"
        && list.all(script_inputs, fn(input: Input) -> Bool {
          input.output.datum == InlineDatum("Banka")
        })
      }
    }
  }
  fn spend(_datum , redeemer: MintingAction, ctx: ScriptContext) -> Bool {
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
        
        // Find the input, that contains the unArxh token.
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

        // Find the output, that contains the unArxh token.
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

        // Find the output, that contains the proposal token.
        expect Some(proposal_output) = list.find(outputs, fn(output: Output) {
          let q = value.quantity_of(output.value, own_policy, bytearray.concat("proposal_", bytearray.from_string(string.from_int(count_b4))))

          if (q == 1) {
            trace "proposal out"
            True
          }else {
            trace "proposal not out"
            False
          }
        }) 

        let Output { datum: proposal_datum, value: out_value, .. } = proposal_output
        expect InlineDatum(prpsl_datum) = proposal_datum
       
        expect Metadata { proposal, state, amount }: Metadata = prpsl_datum

        // Check that the state field, in the datum of the proposal datum is set to "INIT"
        state == "INIT"
        // Check that the count field, in the datum of the proposal datum, has increased
        && (count_b4 == count_after - 1)? 
        // Check that the minted tokens have the correct names
        && (unlearn.named_correct(value.flatten(mint), bytearray.concat("proposal_", bytearray.from_string(string.from_int(count_b4))), own_policy))? 
      }
      Update -> {
        trace "hi"
        // Check that there is only one burned asset with the policy of the script 
        expect [burn_ass] = list.filter(value.flatten(mint), fn(ass: (value.PolicyId, ByteArray, Int)) -> Bool {
        let (policy, _, amt) = ass

        policy == own_policy && amt == -1
      })

        // Check that there is only one input from the validator
        expect [own_input] = list.filter(inputs, fn(input) -> Bool {
          input.output.address.payment_credential == credential.ScriptCredential(own_policy)
        })

        // Check that there is only one output going back to the validator
        expect [own_output] = list.filter(outputs, fn(output) -> Bool {
          output.address.payment_credential == credential.ScriptCredential(own_policy)
        })


        let Output {value: out_value, datum: in_datum ..} = own_input.output
        let Output {value: out_value, datum: out_datum ..} = own_output

        expect InlineDatum(in_dtm) = in_datum
        expect InlineDatum(out_dtm) = out_datum

        expect Metadata { proposal: in_proposal, state: in_state, amount: in_amount }: Metadata = in_dtm
        expect Metadata { proposal: out_proposal,  state: out_state, amount: out_amount }: Metadata = out_dtm
        
        // Check that these values havent changed
        (in_proposal == out_proposal)? 
        && (in_amount == out_amount)?
        // Check that the state of the input was "INIT"
        && (in_state == "INIT")?
        // Check that the state has correctly been updated
        && (out_state == "COMPLETE" || out_state == "CANCELLED")
        // Check that the policies and names of the burned and outputed token are the same, if not for the _R ending
        && unlearn.valEq("_R", burn_ass, value.flatten(out_value))
      }
      Withdraw -> {
        trace "hi"

        // Check that there is only one burned asset with the policy of the script 
        expect [burn_ass] = list.filter(value.flatten(mint), fn(ass: (value.PolicyId, ByteArray, Int)) -> Bool {
          let (policy, _, amt) = ass

          policy == own_policy && amt == -1
        })

        // Find the reference input that has the same policy and name with the burned asset
        let ref_input = txInfo.reference_inputs
          |> list.map(_, fn(input: Input) {
            let Input {output, ..} = input

            output
          })
          |> list.filter(_, fn(output: Output) -> Bool {
            let Output {value, ..} = output

            unlearn.valEq("_Claim", burn_ass, value.flatten(value))
          })

        // Check that the refence input is one
        expect [ref_input] = ref_input

        expect InlineDatum(ref_datum) = ref_input.datum
        
        expect Metadata {amount, state, ..}: Metadata = ref_datum
              
        // Find the input's output that contains the proposal-x-Claim that is burned. 
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

        let script_inputs = list.filter(inputs, fn(input: Input) -> Bool {
          input.output.address.payment_credential ==  credential.ScriptCredential(own_policy)
        })

        // Find the input from the address that burns the proposal-x-Claim
        expect [initiator_input] = list.filter(inputs, fn(input: Input) -> Bool {
          input.output.address.payment_credential == burn_input_output.address.payment_credential
        })

        // Find the output from the address that burns the proposal-x-Claim
        expect [initiator_output] = list.filter(outputs, fn(output: Output) -> Bool{
          output.address.payment_credential == burn_input_output.address.payment_credential
        })
        
        // Find the values in lovelace in the input and the output.
        let user_lvlc_input = value.lovelace_of(initiator_input.output.value)
        let val_from_outputs = value.lovelace_of(initiator_output.value)
        
        trace "lvlvc input"
        trace cbor.diagnostic(user_lvlc_input + amount)
        trace "lvlc output"
        trace cbor.diagnostic(val_from_outputs)
        trace cbor.diagnostic(val_from_outputs)

        // The output to the address, is equal with the amount of lovelace inputted from the address plus the amount stated on the proposal-X datum.
        (val_from_outputs == user_lvlc_input + amount)?
        // Check that the state in proposal-x's datum is "COMPLETE"
        && (state == "COMPLETE")? 
        // Check that all the inputs from the script address have the datum "Banka"
        && list.all(script_inputs, fn(input: Input) -> Bool {
          input.output.datum == InlineDatum("Banka")
        })
      }
    } 
  }
}
