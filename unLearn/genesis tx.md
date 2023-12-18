In the [[genesis tx]], a specific UTXO is used to create an [[unLearn/unLearn]] instance. After the utxo is spent, no other similar utxo can exist. Ensuring the uniqueness of the [[unLearn/unLearn]] instance.

In this transaction the [[unApxn]] and [[members]] NFTs are minted.

[[unApxn]] NFT is locked in the [[Treasury]]

[[members]] NFT is locked in the [[Consensus]]

The datum of [[unApxn]] is initialized, for this [[unLearn/unLearn]] instance:
	[[x]] is set to 0.
	The address of the connected [[Consensus]] is provided by the creator of this [[unLearn/unLearn]] instance.

The datum of [[members]] is initialized, with a user provided list of addresses.

The redeemer needs to have the [[Apxn]] [[Action]] 