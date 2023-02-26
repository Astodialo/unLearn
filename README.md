unLearn: A direct democracy initiative on Cardano.

unLearn'ss idaniko NFT contains the manifesto, the goal we trie to achieve. The users are requested to join or create the organisation that resonates with their ideals and they are willing to work and vote to further that goal. Only the community can decide whether to approve a funding in a fair way, no coin weighted bs. In the proposal, the amount of the funding is going to be decided by the community in a modular proposal creation and voting tool. All the initiatives started from the DAO will have a validation NFT to go with, and validate the originality of the product. The income from the initiatives is going to be locked in the Treasury Validator and the proposer will be able to receive the funding, previously agreed upon with the community, with a voucher NFT.

The goal of unLearn is to give people's work purpose. The purpose is promoting direct democracy. Let's not work another hour for a boss. Let's work for freedom. unLearn what they taught you about work. This is the unArxh...

unLearn is an experiment in work. Great things are going to be created when we start working for our ideals again. The new age of Enlightenment? Do you beleive you would be more productive working for your ideal or for the greed of a boss? With unLearn our ideal is our boss. We work to spread direct democracy. Let's make it sustainable. Let's let our creativity run wild. Let's unLearn...

The Idea of the governance system is that anyone can create a question/proposal, like in a forum. The Question is going to be taken from the user from the frontend and then it will be written in the metadata. The metadata are in the datum (CIP-68 inspired) of an NFT (name: proposal-x) which is going to be minted by the proposer and then locked in a validator script through a multi-sig transaction. In the same transaction a second NFT (name: proposal-x_A) is going to be minted and sent to the proposer. A third NFT (name: proposal-x_R) is also minted in the transaction and sent to the person/group/script responsible for the voting process. If the proposal has the type* value equal "Funding" then a fourth NFT is minted (name: proposal-x_Claim) and is sent t the proposer.

In the first proposal that is minted, the genesis proposal, two more NFTs will be minted the unArxh NFT and the idaniko NFT. Those two are going to be locked in a the unArxh validator. For this minting to happen the unArxh validator address must be empty, so that the minting of thos 2 NFTs can only happen once. The unArxh NFT contains in the datum the counter for the proposal NFTs (TxId) and other general information about the DAO. It is the beginning and the history of the DAO so it is going to be updated in every proposal NFT mint. The idaniko NFT is the ideal of the DAO. This NFT uses the basic NFT metadata standard and in the metadata it contains a homage to the ideals the DAO was created from. The idaniko NFT will be locked in the unArxh validator. The validator is going to ensure that the idaniko NFT is never moved from there. The reason is so that the unArxh validator address can never be empty again. So the two genesis NFTs can never be minted again.

The datum now of the NFT (proposal_x) locked in the validator script contains a list of the "metadata". The question, type and name fields are filled.The answers and results fields are empty. The list aslo contains the datumState* which now has the value "INIT" and the datumAmount which is 0.


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
The proposer after interaction with the community (in a forum, discord etc.) will submit the decided uppon answers throught the frontend and then through a multisig transaction the answers will be added to the metadata (datum) of the NFT (proposal-x) locked in the validator script. For that, the NFT (proposal-x) locked in the validator script will need to be sent again and locked in the validator script. To pass the validation: The transaction needs to be signed by the app wallet that creates the multisig transacation. The NFT (proposal-x_A) in the proposer wallet needs to be burned. The NFT (proposal-x) locked in the validator script needs to be locked again in the same validator script. The two NFTS must be equal (proposal-x + '_A' = proposal-x_A)

The datum now of the NFT (proposal_x) locked in the validator script contains the updated metadata with the question, the answers and name fields, filled. The results field is empty and the datumState has the value "VOTE"
```
datumA.set('answers', user_input);

datumState.set('state', 'VOTE');
```
The same happens for the submission of the results. The results will be added to the metadata (datum) of the NFT (proposal-x) locked in the validator script after passing the same validation this time for the results submission NFT (proposal-x_R)

The datum now of the NFT (proposal_x) locked in the validator script contains the updated metadata with the question, the answers, the results and name fields, filled. The datumState has the value "COMPLETE" or "CANCELLED" and in the case of the type field of the proposal being equal to "Funding" the datumAmount field is updated with the amount that was voted for.
```
datumR.set('results', input);

datumState.set('state', 'COMPLETE'/'CANCELED')

datumAmount.set('amount', result_amount)
```
After that the NFT (proposal-x) is locked in the script. If in the datum of the NFT the type field is equal to "Funding" and the state field is equal to "COMPLETE" then the proposer can interact with the treasury script and claim the amount voted for. To claim the amount the proposer must pass the validation. To pass the validation: The proposer must burn the NFT (proposal_x_Claim) The output of the tx must be only one The amount taken from the treasury script must be equal to the amount field in the datum of the NFT (proposal-x) locked in the update script The two NFTS must be equal (proposal-x + '_Claim' = proposal-x_Claim)

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
* Provide option fro different ways of funding
