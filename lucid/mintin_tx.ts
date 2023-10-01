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

const wallet = await lucid.selectWalletFromPrivateKey(await Deno.readTextFile("./stuff/key.sk"));

const { paymentCredential} = lucid.utils.getAddressDetails(
  await lucid.wallet.address()
);

let address = await lucid.wallet.address();

const genesis_utxo = new Constr(0, [
  new Constr(0, [outRef.txHash]),
  BigInt(outRef.index),
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
 
let count = unArxh_datum.fields[0] as bigint;

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

const nu_datum = Data.to(new Constr(0, [count + 1n]));

const mint_redeemer = Data.to(new Constr(1, []));

console.log("minting redeemer:")
console.log(Data.from(mint_redeemer))
console.log("\nold unArxh datum:")
console.log(unArxh_datum)
console.log("\nnew unArxh datum:") 
console.log(Data.from(nu_datum))
console.log("\nproposal datum:") 
console.log(Data.from(prop_datum))
console.log("\nminting addr:")
console.log(minting_address)
console.log("\nupdater script addr:")
console.log(updater_address)
console.log("\nwallet addr:") 
console.log(address)

const mint_tx = await lucid
  .newTx()
  .collectFrom([utxo], mint_redeemer)
  .payToAddressWithData(minting_address, {inline: nu_datum}, {[unArxh]: 1n, lovelace: 10000000n, })
  //.mintAssets({ [unit]: 1n, [res_unit]: 1n, [claim_unit]: 1n,}, mint_redeemer)
  //.payToAddressWithData(updater_address, { inline: prop_datum}, {[unit]: 1n,} )
  //.payToAddress(address, {[res_unit]: 1n,})
  //.payToAddress(address, {[claim_unit]: 1n,})
  .attachMintingPolicy(minting_script)
  .complete()

console.log(mint_tx);
const mint_signedTx = await mint_tx.sign().complete();

const mint_txHash = await mint_signedTx.submit();
console.log(mint_txHash)

