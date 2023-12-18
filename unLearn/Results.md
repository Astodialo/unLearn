The state of the proposal. It is set as INIT when [[proposal_x]] NFT is minted in the [[proposal creation tx]]. It can be updated to either COMPLETE or CANCELLED in the [[results update tx]] by the multisig/member's.

If the state is COMPLETE the proposer or any person holding [[proposal_x_Claim]] can withraw the amount stated in the [[Amount]] field of [[proposal_x]] from the treasury.

Possibly have to change to another state when the funds are withdrawn.