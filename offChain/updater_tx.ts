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

let address = await lucid.wallet.address();

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
//const updater_address = lucid.utils.validatorToAddress(updater_script) 

const policyId = lucid.utils.mintingPolicyToId(minting_script)

const prop_id = prompt("proposal number:");

const unit = policyId + fromText("proposal_") + fromText(prop_id)
const res_unit = unit + fromText("_R")

const [utxo] = await lucid.utxosAtWithUnit(minting_address, unit) 
const [res_utxo] = await lucid.utxosAtWithUnit(address, res_unit)
const addr_utxos = await lucid.utxosAt(address)

const datum = Data.from(utxo.datum!)

const mint_redeemer = Data.to(new Constr(2, []));
const spend_redeemer = Data.to(new Constr(1, [new Constr(2, [])]));

const [name, proposal, results, _state, amount] = datum.fields

const vote = prompt("type y for yes, n for no:")
let state = ""

if (vote == "y") {
  state = fromText("COMPLETE")
} else {
  state = fromText("CANCELED")
}

const nu_datum = Data.to(new Constr(0, [
 name,
 proposal,
 results,
 state,
 amount
])); 

console.log("\nold proposal datum:")
console.log(datum)
console.log("\nnew proposal datum:")
console.log(nu_datum)
console.log(Data.from(nu_datum))
console.log("\nmint redeemer:")
console.log(mint_redeemer)
console.log(Data.from(mint_redeemer))
console.log("\nspend redeemer:")
console.log(spend_redeemer)
console.log(Data.from(spend_redeemer))

const updater_tx = await lucid
  .newTx()
  .collectFrom([utxo], spend_redeemer)
  .collectFrom([res_utxo],)
  .collectFrom(addr_utxos)
  .mintAssets({[res_unit]: -1n,}, mint_redeemer)
  .payToAddressWithData(minting_address, {inline: nu_datum}, {[unit]: 1n,})
  .attachMintingPolicy(minting_script)
  .complete({change: {address: address}, coinSelection: false})
  
const updater_signedTx = await updater_tx.sign().complete();

const mint_txHash = await updater_signedTx.submit();
console.log(mint_txHash)

