The [[Treasury]] validator is responsible for the genesis of an [[unLearn/unLearn]] instance. The proposal minting process, the proposal update and claiming from the treasury.

It can validate 4 [[Action]]:
	[[Apxn]]
	[[Mintin]]
	[[Update]]
	[[Withdraw]]

The treasury validator holds the treasury of the instance. All treasury funds should have the datum "Banka". It also holds [[proposal_x]] of each proposal. In [[proposal_x]]'s datum are stored the information about proposal x.

To update the datum of [[proposal_x]] the NFT needs to be withdrawn from the [[Treasury]] validator and locked again with the updated datum. This happens in the [[results update tx]].

