1. ```aiken build ../onChain``` to create plutus.json
2. ```deno run -A generate-credentials.ts```
3. ```deno run -A genesis_tx.ts``` to mint the genesis token unArxh. Since the script is parametrized, every time the genesis_tx is run a new unArxh token is created with different policyId.
4. ```deno run -A mintin_tx.ts``` to mint proposals for that specific unArxh token
6.  ```deno run -A claim_tx.ts``` a promt will come up asking you for the proposal_id/number. After the input, the amount state on the proposal will be withrawn from the contract.
