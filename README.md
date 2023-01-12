# bc-procrion
The proposal creation part of a voting system on blockchain.

The Idea of the voting system is that anyone can create a question, like in a forum. The Question is going to be taken from the user from the frontend and then it will be written in the metadata. The metadata are in the datum (CIP-68 inspired) of an NFT (name: proposal-x) which is going to be minted by the proposer and then locked in a validator script through a multi-sig transaction. In the same transaction a second NFT (name: proposal-x_A) is going to be minted and sent to the proposer.

The datum now of the NFT (proposal_x) locked in the validator script contains a list of the datumMD* which is the metadata. The question field and name are filled.The answers and results fields are empty. The list aslo contains the datumState* which now has the value "INIT"

*
  const datumMD: Data = new Map<Data, Data>();
  datumMD.set('name', assetName );
  datumMD.set('question', input);
  datumMD.set('answers', []);
  datumMD.set('results', []);

** 
  const datumState: Data = new Map<Data, Data>();
  datumState.set('state', 'INIT');

  const datumMetadata: Data = {
    alternative: 0,
    fields: [datumMD, datumState]
  };


The proposer after interaction with the community will submit the decided uppon answers throught the frontend and then through a multisig transaction the answers will be added to the metadata (datum) of the NFT (proposal-x) locked in the validator script. For that, the NFT (proposal-x) locked in the validator script will need to be sent again and locked in the validator script. To pass the validation:
The transaction needs to be signed by the app wallet that creates the multisig transacation.
The NFT (proposal-x_A) in the proposer wallet needs to be burned.
The NFT (proposal-x) locked in the validator script needs to be locked again in the same validator script.
The two NFTS must be equal (proposal-x + '_A' = proposal-x_A)

The datum now of the NFT (proposal_x) locked in the validator script contains the updated metadata with the question answers  and name fields filled. The results field is empty and the datumState has the value "VOTE" 
