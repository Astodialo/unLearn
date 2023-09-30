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
import { mint_script, genesis_utxo_hash, genesis_utxo_index} from "./genesis.ts"

const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0/",
    "preprodGoGVMAiqZOuhaZiKG2MSm16jW11NtaY3"
  ),
  "Preprod",
);

const wallet = await lucid.selectWalletFromPrivateKey(await Deno.readTextFile("./stuff/key.sk"));

const { paymentCredential} = lucid.utils.getAddressDetails(
  await lucid.wallet.address()
);
let address = await lucid.wallet.address();

const genesis_utxo = new Constr(0, [
  new Constr(0, [genesis_utxo_hash]),
  BigInt(genesis_utxo_index),
]);

const prop_mint = blueprint.validators.find((v) => v.title === "proposal_mint.prop_mint")
const treas = blueprint.validators.find((v) => v.title === "treasury.treasury")
const updat = blueprint.validators.find((v) => v.title === "updater.updater")

const minting_script: MintingPolicy = {
  type: "PlutusV2",
  script: applyParamsToScript(
    prop_mint?.compiledCode,
    [genesis_utxo]),
}; 

const treasury_script: SpendingValidator = {
  type: "PlutusV2",
  script: treas?.compiledCode ,
};

const updater_script: SpendingValidator = { 
  type: "PlutusV2",
  script: updat?.compiledCode,
};

const minting_address = lucid.utils.validatorToAddress(minting_script)
const treasury_address = lucid.utils.validatorToAddress(treasury_script)
const updater_address = lucid.utils.validatorToAddress(updater_script)

const policyId = lucid.utils.mintingPolicyToId(minting_script)

const unArxh = policyId + fromText("unArxh")

const [utxo] = await lucid.utxosAtWithUnit(minting_address, unArxh) 

let unArxh_datum = Data.from(utxo.datum!) as Constr<bigint>;
 
const count = unArxh_datum.fields[0] as bigint;

const unit = policyId + fromText("proposal_" + String(count))
const res_unit = policyId + fromText("proposal_" + String(count) + "_R")
const claim_unit = policyId + fromText("proposal_" + String(count) + "_Claim")

const prop_datum = Data.to(new Constr(0, [
  fromText("proposal_" + String(count)),
  fromText(" "),
  fromText(" "),
  fromText("INIT"),
  0n,
])); 

const nu_count = count + 1n 
const nu_datum = Data.to(new Constr(0, [nu_count]));

const mint_redeemer = Data.to(new Constr(0, []));

const addr_utxos = await lucid.utxosAt(address);

console.log(unArxh_datum)
console.log(Data.from(nu_datum))
console.log(Data.from(prop_datum))
console.log(Data.from(mint_redeemer))
console.log([unArxh, unit, res_unit, claim_unit])
console.log(utxo)
console.log(utxo.address)
console.log(minting_address)
console.log(updater_address)
console.log(address)

const mint_tx = await lucid
  .newTx()
  .collectFrom([utxo], mint_redeemer)
  .payToAddressWithData(minting_address, { inline: nu_datum }, {[unArxh]: 1n, lovelace: 1_000_000n})
  //.mintAssets({ [unit]: 1n, [res_unit]: 1n, [claim_unit]: 1n,}, mint_redeemer)
  //.payToAddressWithData(updater_address, { inline: prop_datum}, {[unit]: 1n, lovelace: 1_000_000n} )
  //.payToAddress(address, {[res_unit]: 1n, lovelace: 1_000_000n})
  //.payToAddress(address, {[claim_unit]: 1n, lovelace: 1_000_000n})
  .attachMintingPolicy(minting_script)
  .complete()

console.log(mint_tx);
const mint_signedTx = await mint_tx.sign().complete();

const mint_txHash = await mint_signedTx.submit();
console.log(mint_txHash)

