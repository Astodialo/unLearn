import type { NextApiRequest, NextApiResponse } from "next";
import {
  AppWallet,
  ForgeScript,
  PlutusScript,
  Transaction,
  KoiosProvider,
  largestFirst,
  keepRelevant,
  readPlutusData,
  resolvePlutusScriptAddress
} from "@meshsdk/core";
import { resolveDataHash } from '@meshsdk/core';
import type { Mint, Data, Unit, Quantity } from "@meshsdk/core";
import { demoMnemonic } from "../../config/wallet";
import {
  bankWalletAddress,
  idanikoMetadata,
  costLovelace,
} from "../../config/mint";
import { prop_mint_code }  from "../../../lucid/params"


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

  const mintingScript: PlutusScript = {
    code: prop_mint_code,
    version: 'V2',
  };
  const minting_addr = resolvePlutusScriptAddress(mintingScript)


  /**
   * TODO: Here you want to select one of your NFT that has not been minted
   */

  const [unArxhUTXO] = await koios.fetchAddressUTxOs(minting_addr, unArxhUnit)

  const datumMetadataCBOR = unArxhUTXO.output.plutusData;
  const datum = readPlutusData(datumMetadataCBOR);
  const [ name, count ] = datum.fields
  
  
  // unArxh NFTs
  
  // --Datum--
  
  // The datum of the uArxh nft that is going to be locked in the unArxh Validator. In the datum of this NFT are located the general data of the dao, like the number of proposals (TxId used for the minting of the proposal NFTs).The mint of it and the idaniko NFT is the genesis of the dao. It is going to be updated in each proposal NFT minting, in a way acting as a couner.

  const unArxhMetadata: Data = {
    alternative: 0,
    fields: [name, count + 1]
  };

  // The datum of an nft that is going to be locked in the unArxh validator. After the first mint where the unArxh validator address is empty, this NFT is going to stay forever locked, ensuring not to be moved on the Plutus validator. The reason is so that there can never be a point after the first mint that someone can mint the unArxh and idaniko NFTs. It will contain in the metadata what the ideals of the dao are.

  const unArxh: Mint = {
    assetName: "unArxh",
    assetQuantity: "1",
    metadata: {name: "unArxh"},
    label: "721",
    recipient: {
      address: minting_addr,
      datum: {value: unArxhMetadata, inline : true}
    },
  };

  // Proposal NFT 
  
  const assetIdPrefix = "proposal-"
  const assetName = `${assetIdPrefix}${count}`;

  //  --Datum--
  const datumN: Data = assetName;

  const datumP: Data = input;

  const datumR: Data = "";
  
  const datumState: Data = 'INIT';

  const datumAmount: Data = 0;

  const datumMetadata: Data = {
    alternative: 0,
    fields: [datumN, datumP , datumR, datumState, datumAmount]
  };
  const redeemer = { 
    data: { alternative: 0, fields: []},
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
  const assetR: Mint = {
    assetName: assetName + "_R",
    assetQuantity: "1",
    metadata: {name: assetName + "_R"},
    label: "721",
    recipient: {
      address: recipientAddress,
    },
  };

  const assetClaim: Mint = {
    assetName: assetName + "_Claim",
    assetQuantity: "1",
    metadata: {name: assetName + "_Claim"},
    label: "721",
    recipient: {
      address: recipientAddress,
    },
  };

  const proposalUnit = buildUnit(policy, assetName);
  const resultsUnit = buildUnit(policy, assetName + "_R");
  const claimUnit = buildUnit(policy, assetName + "_Claim");

  const outputsMap: Map<Unit, Quantity> = new Map<Unit, Quantity>();
  outputsMap.set('lovelace', costLovelace+5000000 );

  const selectedUtxos = keepRelevant(outputsMap, utxos, '15000000',);

  const tx = new Transaction({ initiator: appWallet });
  tx.setTxInputs(selectedUtxos.concat(unArxhUTXO));
  tx.mintAsset(mintingScript, assetQ, redeemer);
  tx.mintAsset(mintingScript, assetR, redeemer);
  tx.mintAsset(mintingScript, assetClaim, redeemer);
  tx.sendAssets(
    { address: minting_addr,
      datum: {
        value: unArxhMetadata,
        inline: true,
      },
    },
    [{unit: unArxhUnit, quantity: "1",},]
  )
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
