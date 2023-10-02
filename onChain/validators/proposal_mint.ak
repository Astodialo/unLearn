use aiken/bytearray
use aiken/list
use aiken/string
use aiken/transaction.{ScriptContext, Transaction, Input, Output, InlineDatum, Mint, Spend, OutputReference, find_input}
use aiken/transaction/value
use aiken/transaction/credential
use unlearn

type State {
  count: Int
}

type MintingState {
  Genesis
  Mintin
}

validator(utxo_ref: OutputReference) {
  fn prop_mint(_datum, redeemer: MintingState, ctx: ScriptContext) -> Bool {
    when redeemer is {
      Genesis -> {
        expect ScriptContext { transaction: txInfo, purpose: Mint(own_policy) } = ctx
        let Transaction {inputs, outputs, mint, ..} = txInfo
        let mint = value.from_minted_value(mint)

        expect list.any(inputs, fn(input) { input.output_reference == utxo_ref })?

        expect [Output {datum: InlineDatum(output_datum), value, .. }] = list.filter(outputs, fn(output) {
          output.address.payment_credential == credential.ScriptCredential(own_policy)
        })

        expect unlearn.value_contains_master_token(mint, own_policy)?

        expect unlearn.value_has_only_master_and_lovelace(value, own_policy)?

        expect output_state: State = output_datum

        let initial_state =  State {count: 0}

        (initial_state == output_state)?
      }
      Mintin -> {
        expect ScriptContext { transaction: txInfo, purpose: Spend(own_reference) } = ctx
        let Transaction {inputs, outputs, mint, ..} = txInfo
        let mint = value.from_minted_value(mint)

        expect Some(own_input) = inputs |> find_input(own_reference)

        let Input {output: Output { address: credential.Address { payment_credential, .. }, .. }, ..} = own_input

        expect credential.ScriptCredential(own_policy) = payment_credential

        expect Some(unArxh_input) = list.find(inputs, fn(input) {
          let q = value.quantity_of(input.output.value, own_policy, unlearn.master_token_name,)
          q == 1 
        })

        expect Some(unArxh_output) = list.find(outputs, fn(output: Output) { 
          let q = value.quantity_of(output.value, own_policy, unlearn.master_token_name,)

          q == 1
        })

        expect InlineDatum(in_dtm) = unArxh_input.output.datum 
        expect InlineDatum(out_dtm) = unArxh_output.datum

        expect State { count: count_b4 }: State = in_dtm 
        expect State { count: count_after }: State = out_dtm 

        //expect [updater_output] =  list.filter(outputs, fn(output: Output) {
        //  output.address == updater_addr
        //})
        //let Output { datum: update_datum, value: out_value, .. } = updater_output
        //expect InlineDatum(updt_datum) = update_datum
       
        //expect Metadata { name, proposal, results, state, amount }: Metadata = updt_datum

        count_b4 == count_after - 1 && 
        unlearn.named_correct(value.flatten(mint), bytearray.concat("proposal_", bytearray.from_string(string.from_int(count_b4))), own_policy) 
        //name == bytearray.concat(name_b4, bytearray.from_string(string.from_int(count_b4))) &&
        //results == "" &&
        //state == "INIT" &&
        //amount == 0
      }
    }
  }
}
