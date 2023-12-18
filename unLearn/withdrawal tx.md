It is a transaction submitted by the initial proposer, or the current holder of the [[proposal_x_Claim]] NFT, to withdraw money from the treasury of the [[unLearn/unLearn]] instance.

The [[proposal_x]] NFT is used as a reference input. In it's datum it contains the [[Amount]] field. The value of that field must be the output from the [[unLearn/unLearn]] treasury, in this transaction.

The [[proposal_x_Claim]] NFT, with the same [[x]], has to be burned in this transaction to ensure the validity of the withrawer.

The redeemer needs to have the [[Withdraw]] [[Action]]