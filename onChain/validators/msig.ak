use aiken/bytearray
use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/string
use aiken/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, ScriptContext, Spend,
  Transaction,
}
use aiken/transaction/credential
use aiken/transaction/value
use unlearn

type VerificationKeyHash =
  Hash<Blake2b_224, credential.VerificationKey>

type ScriptHash =
  Hash<Blake2b_224, credential.Script>

// Type of the signers datum
type Signers {
  addrs: List<VerificationKeyHash>,
}

// Type of the scripts redeemer
type Action {
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

validator(prop_mint_policy: ScriptHash, utxo_ref: OutputReference) {
  fn msig_mint(redeemer: Action, ctx: ScriptContext) -> Bool {
    expect ScriptContext { transaction: txInfo, purpose: Mint(own_policy) } =
      ctx
    let Transaction { inputs, outputs, mint, .. } = txInfo
    let mint = value.from_minted_value(mint)

    when redeemer is {
      Mintin -> {
        // Check if the utxo, that the validator is parametrized with, is an input of the transaction. After the utxo is consumed no other utxo with the same ref will exist, proving that no other unApxn token is minted. 
        expect
          list.any(inputs, fn(input) { input.output_reference == utxo_ref })?

        // Check if there is a single output in the validator address
        expect [Output { datum: InlineDatum(output_datum), .. }] =
          list.filter(
            outputs,
            fn(output) {
              output.address.payment_credential == credential.ScriptCredential(
                own_policy,
              )
            },
          )
        todo
      }
      Update -> False
      Withdraw -> False
    }
  }

  fn msig(redeemer: Action, _datum, ctx: ScriptContext) -> Bool {
    todo
  }
}
