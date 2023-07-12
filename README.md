unLearn: The Network Neighborhood

The mission of unLearn is to empower people to work for a greater purpose. We believe that working for a boss is not the only way to make a living. We invite you to unLearn the traditional ideas of work and join us in the pursuit of freedom. Connect people not only through ideals but through creativity. Being part of unlearn is not only growing through your ideals but in part of a community, let’s build our neighbourhood. This is the unArxh...

Our primary objectives are as follows:

1. Establish a decentralized network neighborhood within the Cardano ecosystem, promoting collaboration, inclusivity, and community participation.
2. Foster a secure environment where participants can freely express their ideas, engage in constructive discussions, and contribute to the growth of the Neighborhood.
3. Ensure fairness in project selection, funding allocation, and decision-making processes, while actively promoting diversity and inclusion among participants.
4. Safeguard communities against harmful actors, misinformation, and unethical practices, promoting accountability and responsible governance.


Our proposed funding mechanism operates as follows:

1. Proposal Submission: Any individual or group can submit a proposal to the DAO. Proposals can be categorized as governance actions or funding proposals.
2. NFT Minting: Once a proposal is submitted, it is minted as a non-fungible token (NFT) on the Cardano blockchain. This NFT serves as a unique identifier for the proposal throughout its lifecycle.
3. Community Interaction: The proposal is shared with the community for interaction and feedback. Participants can provide suggestions, offer funding amounts, and express their opinions through discussion forums or other designated channels.
4. Proposal Update: Based on community input, the proposal is updated to reflect the outcomes, such as suggested funding amounts or modifications to the governance action. These updates are stored within the NFT datum.
5. Voting: Members of the DAO participate in a voting process to determine the acceptance and funding allocation for each proposal. Voting can be conducted through a secure and transparent on-chain voting system.
6. Proposal NFT Update: After the voting process concludes, the proposal NFT is updated with the voting results, including the approved funding amounts or the decision on the governance action.
7. Claiming Funds: The proposer/holder of the proposal NFT can then claim the approved funding amount from the treasury using the proposal NFT as proof of approval.

The governance system allows anyone to create a question or proposal, like in a forum. An NFT is minted by the proposer and locked in a validator script through a multi-sig transaction. In the same transaction, two additional NFTs are minted (proposal-x_A and proposal-x_R) and sent to the proposer and the script responsible for the voting process, respectively. If the proposal type is "Funding", a fourth NFT (proposal-x_Claim) is minted and sent to the proposer.

In the first proposal to be minted, the genesis proposal, two Non-Fungible Tokens (NFTs) will be created: the unArxh NFT and the idaniko NFT. These two NFTs will be locked in the unArxh validator. The unArxh NFT contains the counter for the proposal NFTs (TxId) and other general information about the Decentralized Autonomous Organization (DAO). It is the beginning and the history of the DAO, so it will be updated in every proposal NFT mint. For that reason if the unArxh validator is empty, which will be only before the genesis proposal, the TxId will be set to 0. The idaniko NFT holds thee Neighborhood values.. The idaniko NFT will be locked in the unArxh validator. The validator will ensure that the idaniko NFT is never moved from there. This is to ensure that the unArxh validator address can never be empty again, so that the txId will not be set to 0 and another NFT proposal-0 is minted.

The NFT (proposal_x) locked in the validator script contains a list of "metadata" with the "question", "type", and "name" fields filled in. The "answers" and "results" fields are empty. The list also contains the "datumState" which currently has the value "INIT" and the "datumAmount" which is set to 0.


```
const datumN: Data = new Map<Data, Data>();
datumN.set('name', assetName );

const datumT: Data = new Map<Data, Data>();
datumT.set('type', user_input) // It can either be a simple proposal or proposal for funding etc. 

const datumQ: Data = new Map<Data, Data>();
datumQ.set('question', user_input2);

const datumA: Data = new Map<Data, Data>();
datumA.set('answers', []);

const datumR: Data = new Map<Data, Data>();
datumR.set('results', []);
  
  const datumState: Data = new Map<Data, Data>();
  datumState.set('state', 'INIT');

  const datumAmount: Data = new Map<Data, Data>();
  datumAmount.set('amount', 0);


  const datumMetadata: Data = {
      alternative: 0,
        fields: [datumN, datumT, datumQ, datumA, datumR, datumState, datumAmount]
  };
 
 ```

After interacting with the community in a forum or Discord, the proposer will submit the agreed-upon answers through the frontend. This will be done via a multisig transaction, which will add the answers to the metadata (datum) of the NFT (proposal-x) locked in the validator script. To pass the validation, the transaction must be signed by the app wallet that creates the multisig transaction. Additionally, the NFT (proposal-x_A) in the proposer wallet must be burned, and the NFT (proposal-x) locked in the validator script must be locked again in the same validator script. The two NFTS must be equal (proposal-x + '_A' = proposal-x_A).

The NFT (proposal_x) locked in the validator script now contains updated metadata with the question, answers, and name fields filled. The results field is empty, and the datumState has the value "VOTE".
```
datumA.set('answers', user_input);

datumState.set('state', 'VOTE');
```
The same happens for the submission of the results. The results will be added to the metadata (datum) of the NFT (proposal-x) locked in the validator script after passing the same validation this time for the results submission NFT (proposal-x_R)

The NFT (proposal_x) locked in the validator script now contains updated metadata with the question, answers, results, and name fields filled in. The datumState has a value of either "COMPLETE" or "CANCELLED", and if the type field of the proposal is equal to "Funding", the datumAmount field is updated with the amount that was voted for.
```
datumR.set('results', input);

datumState.set('state', 'COMPLETE'/'CANCELED')

datumAmount.set('amount', result_amount)
```
If the type field in the data of the NFT (proposal-x) locked in the script is equal to "Funding" and the state field is equal to "COMPLETE", the proposer can interact with the treasury script and claim the amount voted for. To do so, the proposer must pass the validation by burning the NFT (proposal_x_Claim). The output of the transaction must be only one, and the amount taken from the treasury script must be equal to the amount field in the data of the NFT (proposal-x) locked in the script. Additionally, the two NFTS must be equal (proposal-x + '_Claim' = proposal-x_Claim).

TODO
* Better Minting Process almost done boi
* Change the minting validator so it can use a different appwallet if the current one is hacked. Even if the current wallet is hacked there is not a way to get any funds from the DOA since it would still go through the regular procedure and it can be voted cancelled.
* Always include The 0.Cancell option as a voting choice.
* Implement a collateral that the proposer needs to send when he creates a proposal of X amount
* If datumState is "COMPLETE" then the proposer can get the collateral back, if the datumState is "CANCELED" then it is locked/sent to a treasury
* And the whole voting thing ofc ٩(◕‿◕)۶
* Voting system results must be read as a reference output to update the results (datumR) field
* Frontend work (A lot)
* OffChain forum with the posibility to push a forum post to an onchain proposal
* Provide option for different ways of funding
