import Head from "next/head";
import { CardanoWallet, MeshBadge, useWallet } from "@meshsdk/react";
import { createTransaction, creatUpdateTx, signTransaction, signiUpdateTx } from "../backend";
import { useState } from "react";
import { idArray } from "../config/mint"

export default function Home() {
  const { wallet, connected } = useWallet();
  const [txHash, setTxHash] = useState<string | null>(null);
  const [loading, setLoading] = useState<boolean>(false);

  async function startMining() {
    setLoading(true);
    try {
      const recipientAddress = await wallet.getChangeAddress();
      const utxos = await wallet.getUtxos();
      const input = (document.getElementById("user-q") as HTMLInputElement).value;

      const { unsignedTx, originalMetadata } = await createTransaction( recipientAddress,
        utxos,
        input
      );

      const signedTx = await wallet.signTx(unsignedTx, true);

      const { appWalletSignedTx } = await signTransaction(
        signedTx,
        originalMetadata
      );

      const txHash = await wallet.submitTx(appWalletSignedTx);

      setTxHash(txHash);
    } catch (error) {
      console.error(error);
    }
    setLoading(false);
  }

  return (
    <div className="container">
      <Head>
        <title>Mesh App on Cardano</title>
        <meta name="description" content="A Cardano dApp powered my Mesh" />
        <link
          rel="icon"
          href="https://meshjs.dev/favicon/favicon-32x32.png"
        />
        <link
          href="https://meshjs.dev/css/template.css"
          rel="stylesheet"
          key="mesh-demo"
        />
      </Head>

      <main className="main">
        <h1 className="title">
          <a href="https://meshjs.dev/">Mesh</a> Multi-sig Minting
        </h1>

        <div className="demo">
          {connected ? (
            <div>
              <input type="text" id="user-q"/>
              <button
                type="button"
                onClick={() => startMining()}
                disabled={loading}
              >
                {loading ? "Creating transaction..." : "Mint Proposal"}
              </button>
            </div>
          ) : (
            <CardanoWallet />
          )}
          {txHash && (
            <div>
              <p>Successful, transaction hash:</p>
              <code>{txHash}</code>
            </div>
          )}
        </div>

        <div className="grid">
          <a href="https://meshjs.dev/apis" className="card">
            <h2>Documentation</h2>
            <p>
              Our documentation provide live demos and code samples; great
              educational tool for learning how Cardano works.
            </p>
          </a>

          <a
            href="https://meshjs.dev/guides/multisig-minting"
            className="card"
          >
            <h2>Multi-sig minting guide</h2>
            <p>
              Learn more about multi-sig transactions, and how you can create a
              site for minting native tokens.
            </p>
          </a>

          <a href="https://meshjs.dev/react" className="card">
            <h2>React components</h2>
            <p>
              Useful React UI components and hooks, seamlessly integrate them
              into your app, and bring the user interface to life.
            </p>
          </a>
        </div>
      </main>

      <footer className="footer">
        <MeshBadge dark={true} />
      </footer>
    </div>
  );
}
