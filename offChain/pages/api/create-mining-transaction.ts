import type { NextApiRequest, NextApiResponse } from "next";
import {
  AppWallet,
  ForgeScript,
  Transaction,
  KoiosProvider,
  largestFirst,
  keepRelevant,
  readPlutusData
} from "@meshsdk/core";
import { resolveDataHash } from '@meshsdk/core';
import type { Mint, Data, Unit, Quantity } from "@meshsdk/core";
import { demoMnemonic } from "../../config/wallet";
import {
  bankWalletAddress,
  idanikoMetadata,
  costLovelace,
} from "../../config/mint";


export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse
) {
  const recipientAddress = req.body.recipientAddress;
  const utxos = req.body.utxos;
  const policy = 'd9312da562da182b02322fd8acb536f37eb9d29fba7c49dc17255527'
  const input = req.body.input
  
  function toHex(str:String){
    var result = ''
    for (var i = 0; i < str.length; i++) {
      result += str.charCodeAt(i).toString(16)
    }
    return result
  }

  function buildUnit (policy:String, assetName:String) {
    return policy + toHex(assetName)
  }

  const unArxhUnit = buildUnit(policy, 'unArxh');
  const idanikoUnit = buildUnit(policy, 'idaniko')

  const koios = new KoiosProvider('preprod');

  const appWallet = new AppWallet({
    networkId: 0,
    fetcher: koios,
    submitter: koios,
    key: {
      type: "mnemonic",
      words: demoMnemonic,
    },
  });

  const appWalletAddress = appWallet.getPaymentAddress();
  const forgingScript = ForgeScript.withOneSignature(appWalletAddress);


  /**
   * TODO: Here you want to select one of your NFT that has not been minted
   */

  const unArxhUTXO = await koios.fetchAddressUTxOs( 'unArxi validator addr' )

  const assetId = "0"
  /*if (unArxhUTXO.length !== 0) {
      const scriptUtxos = await koios.fetchAddressUTxOs('unArxh validator addr', 'unArxi assetId')
      const utxo = scriptUtxos[0];
      const datumMetadataCBOR = utxo.output.plutusData;
      const datumMetadata = readPlutusData(datumMetadataCBOR);
      const [ assetId ] = datumMetadata.fields
  }
  */
  // unArxh NFTs
  
  // --Datum--
  
  // The datum of the uArxh nft that is going to be locked in the unArxh Validator. In the datum of this NFT are located the general data of the dao, like the number of proposals (TxId used for the minting of the proposal NFTs).The mint of it and the idaniko NFT is the genesis of the dao. It is going to be updated in each proposal NFT minting, in a way acting as a couner.
  const unArxh_assetId: Data = new Map<Data, Data>();
  unArxh_assetId.set('Id', assetId+1)

  const unArxhMetadata: Data = {
    alternative: 0,
    fields: [unArxh_assetId]
  };

  // The datum of an nft that is going to be locked in the unArxh validator. After the first mint where the unArxh validator address is empty, this NFT is going to stay forever locked, ensuring not to be moved on the Plutus validator. The reason is so that there can never be a point after the first mint that someone can mint the unArxh and idaniko NFTs. It will contain in the metadata what the ideals of the dao are.

  const idaniko_ena: Data = new Map<Data, Data>();
  idaniko_ena.set('ena', 'leuteria')

  const idanikoDatum: Data = {
    alternative: 0,
    fields: [idaniko_ena]
  }

  // The minting of the unArxh genesis Nft

  const unArxh: Mint = {
    assetName: "unArxh",
    assetQuantity: "1",
    metadata: {name: "unArxh"},
    label: "721",
    recipient: {
      address: recipientAddress,
      datum: {value: unArxhMetadata, inline : true}
    },
  };

  // The miniting of the idaniko Nft

  const idaniko: Mint = {
    assetName: "idaniko",
    assetQuantity: "1",
    metadata: {idanikoMetadata},
    label: "721",
    recipient: {
      address: recipientAddress,
      datum: {value: idanikoDatum, inline : true}
    },
  };

  
  // Proposal NFT 
  
  const assetIdPrefix = "proposal-"
  const assetName = `${assetIdPrefix}${assetId}`;

  //  --Datum--
  const datumN: Data = new Map<Data, Data>();
  datumN.set('name', assetName );

  const datumT: Data = new Map<Data, Data>();
  datumT.set('type', '') // It can either be a simple proposal or proposal for funding etc. 

  const datumQ: Data = new Map<Data, Data>();
  datumQ.set('question', input);

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

  // Minting for reference NFT to be sent and locked in the validator with the datum containing the data of the progress. At this stage the fields Name, Type, Question, State are filled. The other are null.  
  const assetQ: Mint = {
    assetName: assetName,
    assetQuantity: "1",
    metadata: {name: assetName},
    label: "721",
    recipient: {
      address: recipientAddress,
      datum: {value: datumMetadata, inline : true}
    },
  };

  // Minting for user Answers NFT to be sent to the user/proposer to be letter burned in a multisig tx to update the answers field in the refence NFT's datum. 
  const assetA: Mint = {
    assetName: assetName + "_A",
    assetQuantity: "1",
    metadata: {name: assetName + "_A"},
    label: "721",
    recipient: {
      address: recipientAddress,
    },
  };

  const proposalUnit = buildUnit(policy, assetName);
  const answersUnit = buildUnit(policy, assetName + "_A");

  const outputsMap: Map<Unit, Quantity> = new Map<Unit, Quantity>();
  outputsMap.set('lovelace', costLovelace+5000000 );

  const selectedUtxos = keepRelevant(outputsMap, utxos, '15000000',);

  const tx = new Transaction({ initiator: appWallet });
  tx.setTxInputs(selectedUtxos);
  tx.mintAsset(forgingScript, assetQ,);
  tx.mintAsset(forgingScript, assetA,);
  tx.mintAsset(forgingScript, unArxh,);
  tx.mintAsset(forgingScript, idaniko,);
  tx.sendLovelace(recipientAddress, costLovelace);
  tx.setChangeAddress(recipientAddress);

  const unsignedTx = await tx.build();

  const originalMetadata = Transaction.readMetadata(unsignedTx);

  /**
   * TODO: Here you want to save the `originalMetadata` in a database with the `assetName`
   */

  const maskedTx = Transaction.maskMetadata(unsignedTx);

  // In this starter template, we send `originalMetadata` to the frontend.
  // Not recommended, its better to save the `originalMetadata` in a database.
  res.status(200).json({ unsignedTx, originalMetadata });
}
