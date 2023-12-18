An NFT that is minted during the proposal creation stage, together with [[proposal_x]] and [[proposal_x_Claim]]. 

It is sent to the [[Consensus]] .

It is burnt in the [[results update tx]] to update [[proposal_x]]'s STATE field with the results of the voting.

- The address where it is sent can be tied with each [[unLearn/unLearn]] instance. For example it can be a multisig address where it is passed by the person creating the [[genesis tx]] and is written in the datum of the [[unApxn]] NFT. That would allow us to update that field in the future, with the address of the [[Consensus]]
