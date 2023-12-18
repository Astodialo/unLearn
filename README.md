unLearn at its current implementation is a modular proposal based community management framework. It is meant to allow complex societal relationships to form between self organizing entities, either that be multisigs, or a confederation of multisigs and other governance scripts, like coin weighted voting, other unLearn instances, etc. The proposal part is available, open and permissionless for everyone. For the results consensus needs to be reached from a mutable members list, that each unLearn instance is connected with.

This members list can be updated, through proposals, to add, or remove members. A members list can be a few addresses of certain individuals, maybe a newly formed group, that would function basically like a multisig.

![Image](https://raw.githubusercontent.com/astodialo/unlearn/main/assets/img/multisig unLearn.png)

As well it allows for the same group to change this list and as it grows the members could be a few multisigs, or other unLearn instances, or any other script address voted by the community. With this we seek to empower communities to form and grow on their own but also create connections with who they wish. Creating a network of small interconnected groups to form a network neighbourhood, considered as a cultural sphere of influence between cooperating teams that have a common Treasury and a members list with the member addresses that need to reach consensus.

![Image](https://raw.githubusercontent.com/astodialo/unlearn/main/assets/img/unLearn members transition.png)

From that, further coalitions can happen of those network neighborhoods, creating an entity that could be considered an autonomous self-organized city on blockchain. Instead of centralizing to avoid the complexity under a state, either that be a nation or network, we should embrace that diversity and inherent complexity that comes with human organization and provide a framework for humanity to express itself. A network city governed by a coalition of its members neighbourhood councils of diverse in their governing methods, interests, cultures or any other reason groups would form and decide to represent that voice in that new paradigm of a city.

The base of this organization system is the proposal system. The system is designed with composability and interoperability in mind, to facilitate diverse consensus reaching mechanisms, or proposal minting ones. This is facilitated by the unApxn NFT, which is minted in the genesis tx of each unLearn instance. It is locked in the Treasury and in its datum it contains the addresses of the components of the treasury and other information about the organization. The validator ensures that unApxn can be updated only after a proposal has been approved, meaning consensus has been reached.

As stated anyone can make a proposal, through the proposal creation tx. The proposal and amount need to be inputted by the proposer. In this transaction 3 NFTs will be minted and aswell the state of unApxn will be updated, since it also acts as a counter for the proposals. The NFTs minted are:

proposal_x: It is locked in the proposal minting validator and is the state of the proposal.
proposal_x_R: It is locked in the Consensus validator and is used to update the results, when consensus is reached, in the state of the proposal(proposal_x).
proposal_x_Claim: It is received by the proposer and it acts as a voucher with which they can claim the amount after the proposal state is set to complete, meaning consensus on approving it has been reached. If consensus is not reached then it is basically useless.

