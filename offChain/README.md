1. ```aiken build ../onChain``` to create plutus.json
2. ```deno run -A generate-credentials.ts``` Don't forget to fund the new address (I did)
3. ```deno run -A genesis_tx.ts``` to mint the genesis token unArxh. Since the script is parametrized, every time the genesis_tx is run a new unArxh token is created with different policyId.
4. ```deno run -A mintin_tx.ts``` to mint proposals for that specific unArxh token. You will be prompted for the proposal and the amount. With this transaction 200 ada are also sent to test the claim funcionality. It is not needed.
5. ```deno run -A updater_tx.ts``` to update the state of the proposal to either COMPLETE by pressing y on the prompt either CANCELED by pressing n
6.  ```deno run -A claim_tx.ts``` a promt will come up asking you for the proposal_id/number. After the input, the amount stated on the proposal will be withrawn from the contract.
