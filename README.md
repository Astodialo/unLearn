# bc-procrion
The proposal creation part of a voting system on blockchain.

The Idea of the voting system is that anyone can create a question, like in a forum. The Question is going to be taken from the user from the frontend and then it will be written in the metadata. The metadata are in the datum (CIP-68 inspired) of an NFT (name: proposal-x) which is going to be minted by the proposer and then locked in a validator script through a multi-sig transaction. In the same transaction a second NFT (name: proposal-x_A) is going to be minted and sent to the proposer.

The datum now of the NFT (proposal_x) locked in the validator script contains a list of the "metadata". The question, type and name fields are filled.The answers and results fields are empty. The list aslo contains the datumState* which now has the value "INIT"

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

The proposer after interaction with the community will submit the decided uppon answers throught the frontend and then through a multisig transaction the answers will be added to the metadata (datum) of the NFT (proposal-x) locked in the validator script. For that, the NFT (proposal-x) locked in the validator script will need to be sent again and locked in the validator script. To pass the validation:
The transaction needs to be signed by the app wallet that creates the multisig transacation.
The NFT (proposal-x_A) in the proposer wallet needs to be burned.
The NFT (proposal-x) locked in the validator script needs to be locked again in the same validator script.
The two NFTS must be equal (proposal-x + '_A' = proposal-x_A)

The datum now of the NFT (proposal_x) locked in the validator script contains the updated metadata with the question, the answers and name fields, filled. The results field is empty and the datumState has the value "VOTE" 


```
datumA.set('answers', user_input);

datumState.set('state', 'VOTE');

```

# TODO

* Mint another nft (proposal_x_R) that would be responsible for validation, to sumbit the answers and update the datum accordingly
* The datumState would be updated to be either "COMPLETE" if a decision has been made or "CANCELED" if for any reason the community decided to (for example if community asnwers werent posted in the answers update phase) vote to get cancelled.
* Implement a collateral that the proposer needs to send when he creates a proposal of X amount
* If datumState is "COMPLETE" then the proposer can get the collateral back, if the datumState is "CANCELED" then it is locked/sent to a treasury

* Add a datumAmount  field in the datumMetadata list. The field  would first be empty until the results are submitted. If in the voted upon result is an amount then the datumAmount field will contain that value.
* Add a proposalType field in the datumMD, which will be either "Proposal" || "Funding".
* If the proposalType is "Proposal" there will be no change and the datumAmount field will remain empty after the results are submitted.
* If the proposalType is of type "Funding" then the answers need to be sumbited in a different way containing the amount that then will be writen in the datumAmount after the results are submitted.
* There would then be another NFT minted in the initial tx (proposal-x_Claim).
* The proposer can then claim the voted amount written in the datumAmount field of the datum of the refernce NFT (proposal_x). For that the proposer should claim the amount from the treasury which is locked in a validator script. To pass validation the proposer through a multisig tx will have to burn the NFT (proposal-x_Claim) in his wallet. It will also have to validate that the amount in the output that the proposer takes is equal to the datumAmount field, referenced from the datum of the NFT (proposal_x) locked in the first validator script that is equal to proposal-x_Claim.

* And the whole voting thing ofc ٩(◕‿◕)۶
