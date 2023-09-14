unLearn: A direct democracy initiative on Cardano.

unLearn'ss idaniko NFT contains the manifesto, the goal we try to achieve. Users are encouraged to join or create an organisation that aligns with their values and that they are willing to work and vote for. The community will decide whether to approve a funding proposal in a fair and unbiased manner. The amount of the funding will be determined by the community using a modular proposal creation and voting tool. All initiatives started from the DAO will have a validation Non-Fungible Token (NFT) to verify the originality of the product. The income from the initiatives will be locked in the Treasury Validator and the proposer will be able to receive the agreed-upon funding with a voucher NFT. The rest is going to keep the funding mechanism running for other like minded poeple. So we can all contribute to each others dreams.

The mission of unLearn is to empower people to work for a greater purpose: to promote direct democracy. We believe that working for a boss is not the only way to make a living. We invite you to unLearn the traditional ideas of work and join us in the pursuit of freedom. Connect people not only through ideals but through creativity. Being part of unlearn is not only growing through your ideals but in part of a community, let’s build our neighbourhood. This is the unArxh...

At unLearn, we are experimenting with a new way of working that is driven by our ideals. We believe that when we work for our ideals, we can create something truly great. Could this be the start of a new age of Enlightenment? Do you think you would be more productive if you worked for something you believed in, rather than for the greed of a boss? At unLearn, our ideals are our boss. We are working to spread direct democracy and make it sustainable. We are also encouraging creativity and allowing it to run wild. Let's unLearn...

The governance system allows anyone to create a question or proposal, like in a forum. The question is collected from the user on the frontend and written in the metadata of a CIP-68 inspired NFT (named proposal-x). This NFT is minted by the proposer and locked in a validator script through a multi-sig transaction. In the same transaction, two additional NFTs are minted (proposal-x_A and proposal-x_R) and sent to the proposer and the person/group/script responsible for the voting process, respectively. If the proposal type is "Funding", a fourth NFT (proposal-x_Claim) is minted and sent to the proposer.

In the first proposal to be minted, the genesis proposal, two Non-Fungible Tokens (NFTs) will be created: the unArxh NFT and the idaniko NFT. These two NFTs will be locked in the unArxh validator. The unArxh NFT contains the counter for the proposal NFTs (TxId) and other general information about the Decentralized Autonomous Organization (DAO). It is the beginning and the history of the DAO, so it will be updated in every proposal NFT mint. For that reason if the unArxh validator is empty, which will be only before the genesis proposal, the TxId will be set to 0. The idaniko NFT is the ideal of the DAO. This NFT uses the basic NFT metadata standard and in the metadata it contains our manifesto. The idaniko NFT will be locked in the unArxh validator. The validator will ensure that the idaniko NFT is never moved from there. This is to ensure that the unArxh validator address can never be empty again, so that the txId will not be set to 0 and another NFT proposal-0 is minted.

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
