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

const { paymentCredential} = lucid.utils.getAddressDetails(
  await lucid.wallet.address()
);

let address = await lucid.wallet.address();

const genesis_utxo = new Constr(0, [
  new Constr(0, [outRef.txHash]),
  BigInt(outRef.index),
]);

const prop_mint = blueprint.validators.find((v) => v.title === "proposal_mint.prop_mint")
const updat = blueprint.validators.find((v) => v.title === "updater.updater")

const minting_script: MintingPolicy = {
  type: "PlutusV2",
  script: applyParamsToScript(
    prop_mint?.compiledCode,
    [genesis_utxo]),
}; 

const updater_script: SpendingValidator = { 
  type: "PlutusV2",
  script: updat?.compiledCode,
};

const minting_address = lucid.utils.validatorToAddress(minting_script) 
//const updater_address = lucid.utils.validatorToAddress(updater_script) 

const policyId = lucid.utils.mintingPolicyToId(minting_script)

const unit = policyId + fromText("proposal_0")
const res_unit = policyId + fromText("proposal_0_R")

const [utxo] = await lucid.utxosAtWithUnit(minting_address, unit) 

let datum = Data.from(utxo.datum!) as Constr<[string, string, string, string, bigint]> 

const mint_redeemer = Data.to(new Constr(2, []));

let [name, proposal, results, state, amount] = datum.fields

let nu_datum = Data.to(new Constr(0, [
 name,
 proposal,
 fromText("whatever"), 
 fromText("COMPLETE"),
 100n
])); 

console.log("\nold proposal datum:")
console.log(datum)
console.log("\nnew proposal datum:")
console.log(Data.from(nu_datum))
console.log("\nredeemer:")
console.log(Data.from(mint_redeemer))

const updater_tx = await lucid
  .newTx()
  .collectFrom([utxo], mint_redeemer)
  .mintAssets({[res_unit]: -1n,}, mint_redeemer)
  .payToAddressWithData(minting_address, {inline: nu_datum}, {[unit]: 1n,})
  //.attachSpendingValidator(updater_script)
  .attachMintingPolicy(minting_script)
  .complete()
  
const updater_signedTx = await updater_tx.sign().complete();

const mint_txHash = await updater_signedTx.submit();
console.log(mint_txHash)

