unLearn: modular governance framework

At its core, unLearn embraces the ideas of decentralization, autonomy and self-management, empowering individuals and organizations to collaboratively shape their own collective future. Central to the unLearn freamework is the concept of fluidity and adaptability. Unlike traditional top-down governance models, unLearn starts with a foundation of fluidity, allowing for organic growth and evolution over time.

An roganization can begin by utilizing a multisig, or any other consensus mechanism they'd like to use. This initial stage enables multiple parties to collectively manage resources and make decisions, fostering inclusivity and decentralization from the outset.

![Image](https://raw.githubusercontent.com/astodialo/unlearn/main/assets/img/multisig%20unLearn.png)

As the ecosystem matures, proposals for gevernance enhancements can be introduced amd voted upon by the community. Through this process, a more sophisticated governance structure can emerge, potentially involving a confederation of multisigs and other voting mechanisms.

This confederation acts as a decentralized layer of governance, ensuring transparency and efficiency in decision-making. But the unLearn framework doesn't stop there. It also enables collaboration between different organization throughty the confederation of their blockchain addresses. By pooling resources and aligning their efforts, organizations can work together towards common goals in a decentralized manner.

![Image](https://raw.githubusercontent.com/astodialo/unlearn/main/assets/img/unLearn%20members%20transition.png)

In essence, the unLearn framework represents a new pradigm in governance. one that embraces decentralization, adaptability and collective action. It fosters a sence of community and collective responsibility. By combining principles of libertarion socialism with the technological capabilities of blockchain, unLearn paves the way for a more equitable, transparent and resilient future.

The base of this organization system is the proposal system. The system is designed with composability and interoperability in mind, to facilitate diverse consensus reaching mechanisms, or proposal minting ones. This is facilitated by the unApxn NFT, which is minted in the genesis tx of each unLearn instance. It is locked in the Treasury and in its datum it contains the addresses of the components of the treasury and other information about the organization. The validator ensures that unApxn can be updated only after a proposal has been approved, meaning consensus has been reached.

As stated anyone can make a proposal, through the proposal creation tx. The proposal and amount need to be inputted by the proposer. In this transaction 3 NFTs will be minted and aswell the state of unApxn will be updated, since it also acts as a counter for the proposals. The NFTs minted are:

```proposal_x```: It is locked in the proposal minting validator and is the state of the proposal.
```proposal_x_R```: It is locked in the Consensus validator and is used to update the results, when consensus is reached, in the state of the proposal(proposal_x).
```proposal_x_Claim```: It is received by the proposer and it acts as a voucher with which they can claim the amount after the proposal state is set to complete, meaning consensus on approving it has been reached. If consensus is not reached then it is basically useless.

