import { 
  Lucid,
  Kupmios,
  MintingPolicy,
  SpendingValidator, 
  fromText, 
  applyDoubleCborEncoding,
  Data, 
  TxHash, 
  sign, 
  Constr, 
  fromHex, 
  toHex,
  toLabel,
  OutRef,
  Blockfrost,
  applyParamsToScript
} from "https://deno.land/x/lucid@0.10.7/mod.ts";
import blueprint from "../onChain/plutus.json" assert { type: "json"}
import * as cbor from "https://deno.land/x/cbor@v1.4.1/index.js";

const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0/",
    "preprodGoGVMAiqZOuhaZiKG2MSm16jW11NtaY3"
  ),
  "Preprod",
);

type Genesis = {
  validator: string;
  validatorHash: string;
  validatorAddress: string;
  outRef: { txHash: string; index: number };
};

const genesisFile = Deno.readTextFileSync(`./stuff/genesis.json`);

const {validator, validatorHash, validatorAddress, outRef}: Genesis = JSON.parse(genesisFile); 

const wallet = await lucid.selectWalletFromSeed(await Deno.readTextFile("./stuff/seed"));

const address = await lucid.wallet.address()

const genesis_utxo = new Constr(0, [
  new Constr(0, [outRef.txHash]),
  BigInt(outRef.index),
]);

const prop_mint = blueprint.validators.find((v) => v.title === "proposal_mint.prop_mint")

const minting_script: MintingPolicy = {
  type: "PlutusV2",
  script: applyParamsToScript(
    prop_mint?.compiledCode,
    [genesis_utxo]),
};

const minting_address = lucid.utils.validatorToAddress(minting_script)

const policyId = lucid.utils.mintingPolicyToId(minting_script)

const prop_id = prompt("proposal number:");

const proposal = policyId + fromText("proposal_") + fromText(prop_id)
const claim_unit = proposal + fromText("_Claim")

const [scriptUtxo] = await lucid.utxosAt(minting_address);

const [utxo] = await lucid.utxosAtWithUnit(minting_address, proposal)
const [claim_utxo] = await lucid.utxosAtWithUnit(address, claim_unit)

const datum = Data.from(utxo.datum!) as Constr<[string, string, string, string, bigint]> 

const mint_redeemer = Data.to(new Constr(3, []));
const spend_redeemer = Data.to(new Constr(1, [new Constr(3, [])]));

const amt: bigint = datum.fields[4] 

console.log("withrawal amount:")
console.log(amt)
console.log("\nredeemer:")
console.log(Data.from(mint_redeemer))
console.log(minting_address)
console.log(scriptUtxo.assets)
console.log(claim_utxo.assets.lovelace)

const claim_tx = await lucid
  .newTx()
  .readFrom([utxo])
  .collectFrom([scriptUtxo], spend_redeemer)
  .mintAssets({[claim_unit]: -1n,}, mint_redeemer)
  .payToAddress(address, {lovelace: amt + claim_utxo.assets.lovelace},)
  .attachMintingPolicy(minting_script)
  .complete({change: {address: minting_address}})


const claim_signedTx = await claim_tx.sign().complete();

const claim_txHash = await claim_signedTx.submit();
console.log(claim_txHash)
