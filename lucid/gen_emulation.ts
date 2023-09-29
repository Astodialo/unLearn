import {
  Address,
  applyDoubleCborEncoding,
  applyParamsToScript,
  Constr,
  MintingPolicy,
  Script,
  Data,
  Emulator,
  fromText,
  generatePrivateKey,
  Lucid,
  PrivateKey,
  UTxO,
} from "https://deno.land/x/lucid/mod.ts";

// Imports blueprint generated from `aiken build`
// Blueprints CIP: https://cips.cardano.org/cips/cip57/
import blueprint from "../onChain/plutus.json" assert { type: "json" };

const privateKeyAlice = generatePrivateKey();

const addressAlice = await (await Lucid.new(undefined, "Custom"))
  .selectWalletFromPrivateKey(privateKeyAlice).wallet.address();

const emulator = new Emulator([{
  address: addressAlice,
  assets: { lovelace: 100_000_000n },
}]);

const lucid = await Lucid.new(emulator);

const prop_mint = blueprint.validators.find((v) => v.title === "proposal_mint.prop_mint")

const tokenName = "unArxh";
let policyId, prop_mint_policy, assetName, redeemValidator, lockAddress;

const applyParamsToContract = (outputReference) => {
  prop_mint_policy = applyParamsToScript(
    prop_mint.compiledCode,
    [outputReference,],
  );

  applyParamsToScript
  policyId = lucid.utils.validatorToScriptHash({
    type: "PlutusV2",
    script: prop_mint_policy,
  });

  assetName = `${policyId}${fromText(tokenName)}`;
};

const mintmasterKey = async (minterPrivateKey: PrivateKey) => {
  const owner: Lucid = lucid.selectWalletFromPrivateKey(minterPrivateKey);
  const [utxo] = await owner.wallet.getUtxos();
  //   console.log({ utxo });

  const outRef = new Constr(0, [
    new Constr(0, [utxo.txHash]),
    BigInt(utxo.outputIndex),
  ]);

  applyParamsToContract(outRef);

  const mintRedeemer = Data.to(new Constr(0, []));
  const genesis_datum = Data.to(new Constr(0, [0n]));

  const txHash = await owner.newTx()
    .collectFrom([utxo])
    // use the master_key validator
    .attachMintingPolicy(
      {
        type: "PlutusV2",
        script: applyDoubleCborEncoding(prop_mint_policy),
      },
    )
    // mint 1 of the asset
    .mintAssets(
      { [assetName]: BigInt(1) },
      // this redeemer is the first argument to the master_key validator
      mintRedeemer,
    )
    .payToContract(
      lucid.utils.validatorToAddress(applyDoubleCborEncoding(prop_mint_policy)),
      {
        // On unlock this gets passed to the redeem
        // validator as datum. Our redeem validator
        // doesn't use it so we can just pass in anything.
        inline: genesis_datum,
      },
      { lovelace: BigInt(50_000_000) },
    )
    .complete()
    .then((tx) => tx.sign().complete())
    .then((tx) => tx.submit());
  console.log(txHash);
};
