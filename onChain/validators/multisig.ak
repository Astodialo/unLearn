use aiken/builtin
use aiken/dict
use aiken/bytearray
use aiken/list
use aiken/transaction.{ScriptContext, Transaction, Input, Output, InlineDatum, ScriptPurpose, Spend}
use aiken/transaction/value
use aiken/transaction/credential
use unlearn

type MintingState {
  Mintin
  Genesis
}

validator(utxo_ref: OutputReference, singers: List<ByteArray>  ) {
  fn prop_mint(state: MintingState, ctx: ScriptContext) -> Bool {
    when state is {
      Genesis -> {
        expect ScriptContext { transaction: txInfo, purpose: Mint(own_policy) } = ctx
        let Transaction {inputs, outputs, mint, }
        
        todo
      }
