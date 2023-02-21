# unLearn
The framework for of a voting/community funding system on blockchain.

The Idea of the voting system is that anyone can create a question/proposal, like in a forum. The Question is going to be taken from the user from the frontend and then it will be written in the metadata. The metadata are in the datum (CIP-68 inspired) of an NFT (name: proposal-x) which is going to be minted by the proposer and then locked in a validator script through a multi-sig transaction. In the same transaction a second NFT (name: proposal-x_A) is going to be minted and sent to the proposer. A third NFT (name: proposal-x_R) is also minted in the transaction and sent to the person/group/script responsible for the voting process. If the proposal has the type* value equal "Funding" then a fourth NFT is minted (name: proposal-x_Claim) and is sent t the proposer.

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

  The proposer after interaction with the community (in a forum, discord etc.) will submit the decided uppon answers throught the frontend and then through a multisig transaction the answers will be added to the metadata (datum) of the NFT (proposal-x) locked in the validator script. For that, the NFT (proposal-x) locked in the validator script will need to be sent again and locked in the validator script. To pass the validation:
  The transaction needs to be signed by the app wallet that creates the multisig transacation.
  The NFT (proposal-x_A) in the proposer wallet needs to be burned.
  The NFT (proposal-x) locked in the validator script needs to be locked again in the same validator script.
  The two NFTS must be equal (proposal-x + '_A' = proposal-x_A)

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
  After that the NFT (proposal-x) is locked in the script. If in the datum of the NFT the type field is equal to "Funding" and the state field is equal to "COMPLETE" then the proposer can interact with the treasury script and claim the amount voted for. To claim the amount the proposer must pass the validation.
  To pass the validation:
  The proposer must burn the NFT (proposal_x_Claim)
  The output of the tx must be only one
  The amount taken from the treasury script must be equal to the amount field in the datum of the NFT (proposal-x) locked in the update script
  The two NFTS must be equal (proposal-x + '_Claim' = proposal-x_Claim)

# TODO
* Better Minting Process
* Implement a collateral that the proposer needs to send when he creates a proposal of X amount
* If datumState is "COMPLETE" then the proposer can get the collateral back, if the datumState is "CANCELED" then it is locked/sent to a treasury
(?) Make the claim phase for multiple wallets (?)
* And the whole voting thing ofc ٩(◕‿◕)۶
