import type { NextApiRequest, NextApiResponse } from "next";
import {
  AppWallet,
  ForgeScript,
  Transaction,
  KoiosProvider,
  largestFirst,
  deserializePlutusData,
  fromPlutusData
} from "@meshsdk/core";
import { resolveDataHash } from '@meshsdk/core';
import type { Mint, Data, Asset } from "@meshsdk/core";
import { demoMnemonic } from "../../config/wallet";
import {
  assetsMetadata,
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
  const assetName = req.body.assetName
  const mdAnswers = req.body.mdAnswers

  const scriptAddress = 'test'
  const policy = "test"

  const readPlutusData = (plutusData: string): Data => {
    return fromPlutusData(deserializePlutusData(plutusData));
  };

  function toHex(str:String){
    var result = ''
    for (var i = 0; i < str.length; i++) {
      result += str.charCodeAt(i).toString(16)
    }
    return result
  }

  function buildUnit(policy:String, assetName:String) {
    return policy + toHex(assetName)
  }

  const updateUnit = buildUnit(policy, assetName);
  const burnUnit = buildUnit(policy, assetName.concat('_A'))

  const koios = new KoiosProvider('preview')

  const appWallet = new AppWallet({
    networkId: 0,
    fetcher: koios,
    submitter:  koios,
    key: {
      type: "mnemonic",
      words: demoMnemonic,
    },
  });


  const scriptUtxos = await koios.fetchAddressUTxOs(
    scriptAddress,
    updateUnit
  );
  const utxo = scriptUtxos[0];
  const datumMetadataCBOR = utxo.output.plutusData;
  const datumMetadata = readPlutusData(datumMetadataCBOR);
  
  const [datumN, datumT, datumQ, datumA, datumR, datumState, datumAmount]  = datumMetadata.fields;
  
  datumN.set('answers', mdAnswers);

  datumState.set('state', 'VOTE');

  const appWalletAddress = appWallet.getPaymentAddress();
  const forgingScript = ForgeScript.withOneSignature(appWalletAddress);

  const burnAss : Asset = {
    unit: burnUnit,
    quantity: '1',
  };

  const tx = new Transaction({ initiator: appWallet });
  tx.redeemValue({
      value: utxo,
      script: {
        version: 'V1',
        code : '4e4d01000033222220051200120011'
    },
    datum: datumMetadata,
  });
  tx.sendValue(scriptAddress, utxo);
  tx.burnAsset(forgingScript, burnAss);
  tx.setChangeAddress(scriptAddress);

 const unsignedTx = await tx.build(); 

 res.status(200).json({unsignedTx});
}
