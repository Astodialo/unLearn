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

