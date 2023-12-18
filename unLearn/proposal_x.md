An NFT that is minted during the [[proposal creation tx]], together with [[proposal_x_R]] and [[proposal_x_Claim]].

It is locked in the [[Treasury]].

The datum of the NFT contains information about the proposal:
	[[Proposal]]
	[[Type]]
	[[Results]]
	[[Amount]]

The Proposal, the Type and the Amount are user inputs and are submitted when minting the NFT. The State is initially set to INIT.

To update the State to either COMPLETED or CANCELED, a transaction needs to be created, in which [[proposal_x_R]] is burned to assure validity.

After that, the proposer, can burn [[proposal_x_Claim]] and if [[proposal_x]] State is COMPLETE, they can claim the proposed amount from the treasury.

