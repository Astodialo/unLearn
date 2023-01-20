import type { NextApiRequest, NextApiResponse } from "next";
import {
  AppWallet,
  ForgeScript,
  Transaction,
  KoiosProvider,
  largestFirst,
} from "@meshsdk/core";
import { resolveDataHash } from '@meshsdk/core';
import type { Mint, Data } from "@meshsdk/core";
import { demoMnemonic } from "../../config/wallet";
import {
  idArray,
  bankWalletAddress,
  costLovelace,
} from "../../config/mint";


export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse
) {
  const recipientAddress = req.body.recipientAddress;
  const utxos = req.body.utxos;
  const input = req.body.input

  const blockchainProvider = new KoiosProvider("preview");

  const appWallet = new AppWallet({
    networkId: 0,
    fetcher: blockchainProvider,
    submitter: blockchainProvider,
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

  const assetIdPrefix = "proposal-";
  const assetId = idArray.pop();
  const assetName = `${assetIdPrefix}${assetId}`;

  const datumMD: Data = new Map<Data, Data>();
  datumMD.set('name', assetName );
  datumMD.set('question', input);
  datumMD.set('answers', []);
  datumMD.set('results', []);
  
  const datumState: Data = new Map<Data, Data>();
  datumState.set('state', 'INIT');

  const datumAmount: Data = new Map<Data, Data>();
  datumAmount.set('amount', 0);


  const datumMetadata: Data = {
    alternative: 0,
    fields: [datumMD, datumState]
  };

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

  const assetA: Mint = {
    assetName: assetName + "_A",
    assetQuantity: "1",
    metadata: {name: assetName + "_A"},
    label: "721",
    recipient: {
      address: recipientAddress,
    },
  };


  const selectedUtxos = largestFirst(costLovelace, utxos, true);

  const tx = new Transaction({ initiator: appWallet });
  tx.setTxInputs(selectedUtxos);
  tx.mintAsset(forgingScript, assetQ);
  tx.mintAsset(forgingScript, assetA);
  tx.sendLovelace(bankWalletAddress, costLovelace);
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
