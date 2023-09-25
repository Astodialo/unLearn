use aiken/builtin
use aiken/dict
use aiken/bytearray
use aiken/list
use aiken/string
use aiken/transaction.{ScriptContext, Transaction, Input, Output, InlineDatum, ScriptPurpose, Mint, OutputReference}
use aiken/transaction/value
use aiken/transaction/credential 
use unlearn

type State {
  name: ByteArray,
  count: Int
}

type MintingState {
  Mintin
  Genesis
}

type Metadata {
  name:     ByteArray,
  proposal: ByteArray,
  results:  ByteArray,
  state:    ByteArray,
  amount:   Int
}

validator(utxo_ref: OutputReference ) {
  fn prop_mint(state: MintingState, ctx: ScriptContext) -> Bool {
    when state is {
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

        let initial_state =  State { name: "proposal", count: 0}

        (initial_state == output_state)?
      }
      Mintin -> {
        expect ScriptContext { transaction: txInfo, purpose: Mint(own_policy) } = ctx
        let Transaction {inputs, outputs, mint, ..} = txInfo
        let mint = value.from_minted_value(mint)

        expect Some(input) = list.find(inputs, fn(input) -> Bool {
          expect Some(ass) = input.output.value
          |> value.flatten(_)
          |> list.find(_, fn(ass: (value.PolicyId, value.AssetName, Int)) -> Bool {
            let (input_pid, input_name, input_q) = ass

            input_pid == own_policy && input_name == unlearn.master_token_name && input_q == 1
          }) 
       
          True
        })

        let Output { address: in_address, value: in_value, datum: in_datum, ..} = input.output
        expect [own_output] = list.filter(outputs, fn(output: Output) { 
          output.address == in_address
        })
        let Output { datum: out_datum, value: out_value, .. } = own_output


        expect InlineDatum(base_datum_b4) = in_datum
        expect InlineDatum(base_datum_after) = out_datum

        expect State { name: name_b4, count: count_b4 }: State = base_datum_b4
        expect State { name: name_after, count: count_after }: State = base_datum_after

        expect [updater_output] =  list.filter(outputs, fn(output: Output) {
        output.address == updater_addr
        })
        let Output { datum: update_datum, value: out_value, .. } = updater_output
        expect InlineDatum(updt_datum) = update_datum
       
       expect Metadata { name, proposal, results, state, amount }: Metadata = updt_datum


        name_b4 == name_after &&
        count_b4 == count_after + 1 &&
        unlearn.named_correct(value.flatten(mint), bytearray.concat(name_b4, bytearray.from_string(string.from_int(count_b4))), own_policy) &&
        name == bytearray.concat(name_b4, bytearray.from_string(string.from_int(count_b4))) &&
        results == "" &&
        state == "INIT" &&
        amount == 0
      }
    }
  }
}
