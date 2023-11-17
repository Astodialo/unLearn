use aiken/bytearray
use aiken/cbor
use aiken/list
use aiken/string
use aiken/transaction.{ScriptContext, Transaction, Input, Output, InlineDatum, Mint, Spend, OutputReference, find_input}
use aiken/transaction/value
use aiken/hash.{Blake2b_224, Hash}
use aiken/transaction/credential
use unlearn

type VerificationKeyHash = Hash<Blake2b_224, credential.VerificationKey>

// Type of the unArxh datum
type State {
  count: Int,
  results_addr: VerificationKeyHash 
}

// Type of the scripts redeemer
type Action {
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
  fn gov_aggregator(redeemer: Action, ctx: ScriptContext) -> Bool {
    todo
  }
}
