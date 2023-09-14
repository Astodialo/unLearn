use aiken/transaction.{ScriptContext}
use aiken/transaction/value

validator {
    fn updater(_datum, ctx: ScriptContext) -> Bool {
      let txInfo = ctx.transaction
        let txMint = txInfo.mint 
          |> value.from_minted_value
          |> value.flatten


       todo 
      }
  }
