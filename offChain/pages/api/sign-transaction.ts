import type { NextApiRequest, NextApiResponse } from "next";
import { AppWallet, Transaction, KoiosProvider } from "@meshsdk/core";
import { demoMnemonic } from "../../config/wallet";

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse
) {
  const assetName = req.body.assetName;
  const signedTx = req.body.signedTx;
  const originalMetadata = req.body.originalMetadata;

  const koios = new KoiosProvider("preview");

  const appWallet = new AppWallet({
    networkId: 0,
    fetcher: koios,
    submitter: koios,
    key: {
      type: "mnemonic",
      words: demoMnemonic,
    },
  });

  /**
   * TODO: Here you want to retrieve the `originalMetadata` from database with the `assetName`
   */

  const signedOriginalTx = Transaction.writeMetadata(
    signedTx,
    originalMetadata
  );

  const appWalletSignedTx = await appWallet.signTx(signedOriginalTx, true);

  res.status(200).json({ appWalletSignedTx });
}
