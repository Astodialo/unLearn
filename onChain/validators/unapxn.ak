use aiken/bytearray
use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/string
use aiken/transaction.{
  InlineDatum, Mint, Output, OutputReference, ScriptContext, Spend, Transaction,
}
use aiken/transaction/credential
use aiken/transaction/value
use unlearn

type VerificationKeyHash =
  Hash<Blake2b_224, credential.VerificationKey>

type ScriptHash =
  Hash<Blake2b_224, credential.Script>

// Type of the unApxn datum
type State {
  count: Int,
  minting_addr: ScriptHash,
  results_addr: VerificationKeyHash,
}

// Type of the scripts redeemer
type Action {
  Arxh
  Mintin
}

// Type of the proposal-x datum
type Metadata {
  proposal: ByteArray,
  state: ByteArray,
  amount: Int,
}

validator(utxo_ref: OutputReference) {
  fn unApxn(redeemer: Action, ctx: ScriptContext) -> Bool {
    expect ScriptContext { transaction: txInfo, purpose: Mint(own_policy) } =
      ctx
    let Transaction { inputs, outputs, mint, .. } = txInfo
    let mint = value.from_minted_value(mint)

    when redeemer is {
      Arxh -> {
        // Check if the utxo, that the validator is parametrized with, is an input of the transaction. After the utxo is consumed no other utxo with the same ref will exist, proving that no other unApxn token is minted. 
        expect
          list.any(inputs, fn(input) { input.output_reference == utxo_ref })?

        // Check in the outputs, that only one token that goes to the unApxn validator and that it is the unLearn NFT.
        expect [Output { datum: InlineDatum(output_datum), .. }] =
          list.filter(
            outputs,
            fn(output) {
              output.address.payment_credential == credential.ScriptCredential(
                own_policy,
              ) && value.quantity_of(
                output.value,
                own_policy,
                unlearn.master_token_name,
              ) == 1
            },
          )

        // Check that only one unApxn token is minted
        expect unlearn.value_contains_master_token(mint, own_policy)

        // Check that the count field, in the initial State in the unApxn datum, is 0.
        expect State { count, minting_addr: _m_addr, results_addr: _r_addr }: State =
          output_datum

        (count == 0)?
      }
      Mintin -> False
    }
  }

  fn unApxn_spend(_datum, redeemer: Action, ctx: ScriptContext) -> Bool {
    expect ScriptContext { transaction: txInfo, purpose: Spend(own_reference) } =
      ctx
    let Transaction { inputs, outputs, mint, .. } = txInfo
    let mint = value.from_minted_value(mint)
    let own_policy = unlearn.policy_from_reference(own_reference, inputs)

    when redeemer is {
      Arxh -> False
      Mintin -> {
        // Check that there is only one input from the validator and that it is the unApxn token.
        expect [unApxn_input] =
          list.filter(
            inputs,
            fn(input) -> Bool {
              let q =
                value.quantity_of(
                  input.output.value,
                  own_policy,
                  unlearn.master_token_name,
                )

              input.output.address.payment_credential == credential.ScriptCredential(
                own_policy,
              ) && q == 1
            },
          )

        // Find the output, that contains the unApxn token.
        expect Some(unApxn_output) =
          list.find(
            outputs,
            fn(output: Output) {
              let q =
                value.quantity_of(
                  output.value,
                  own_policy,
                  unlearn.master_token_name,
                )

              q == 1
            },
          )

        //q == 1
        expect InlineDatum(in_dtm) = unApxn_input.output.datum
        expect InlineDatum(out_dtm) = unApxn_output.datum

        expect State {
          count: count_b4,
          minting_addr: ma_b4,
          results_addr: ra_b4,
        }: State = in_dtm
        expect State {
          count: count_after,
          minting_addr: ma_after,
          results_addr: ra_after,
        }: State = out_dtm
        // Find the output, that contains the proposal token.
        expect Some(proposal_output) =
          list.find(
            outputs,
            fn(output: Output) {
              let q =
                value.quantity_of(
                  output.value,
                  ma_b4,
                  bytearray.concat(
                    "proposal_",
                    bytearray.from_string(string.from_int(count_b4)),
                  ),
                )

              q == 1
            },
          )

        let Output { datum: proposal_datum, value: _out_value, .. } =
          proposal_output
        expect InlineDatum(prpsl_datum) = proposal_datum
        expect Metadata { state, .. }: Metadata = prpsl_datum

        // Find the output, that contains the proposal_R token.
        expect Some(result_output) =
          list.find(
            outputs,
            fn(output: Output) {
              let q =
                value.quantity_of(
                  output.value,
                  ma_b4,
                  bytearray.concat(
                    bytearray.concat(
                      "proposal_",
                      bytearray.from_string(string.from_int(count_b4)),
                    ),
                    "_R",
                  ),
                )

              q == 1
            },
          )

        // Check that the state field, in the datum of the proposal datum is set to "INIT"
        state == "INIT" && // Check that the count field, in the datum of unApxn NFT, has increased
        (count_after - 1 == count_b4)? && // Check that the minting_addr field, in the datum of unApxn NFT, is the same
        (ma_after == ma_b4)? && // Check that the results_addr field, in the datum of unApxn NFT, is the same
        (ra_after == ra_b4)? && // Check that the minted tokens have the correct names
        unlearn.named_correct(
          value.flatten(mint),
          bytearray.concat(
            "proposal_",
            bytearray.from_string(string.from_int(count_b4)),
          ),
          ma_after,
        )? && // Check that proposal_X is sent to the address stated in the unApxn token's datum as minting_addr.
        proposal_output.address.payment_credential == credential.ScriptCredential(
          ma_after,
        ) && // Check that proposal_x_R is sent to the address stated in the unApxn token's datum as results_addr.
        (result_output.address.payment_credential == credential.VerificationKeyCredential(
          ra_after,
        ))?
      }
    }
  }
}
