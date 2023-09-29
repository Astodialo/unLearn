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

const wallet = await lucid.selectWalletFromPrivateKey(await Deno.readTextFile("./key.sk"));

const { paymentCredential} = lucid.utils.getAddressDetails(
  await lucid.wallet.address()
);
const address = lucid.wallet.address();

const genesis_utxo = new Constr(0, [
  new Constr(0, [genesis_utxo_hash]),
  BigInt(genesis_utxo_index),
]);

const prop_mint = blueprint.validators.find((v) => v.title === "proposal_mint.prop_mint")
const treas = blueprint.validators.find((v) => v.title === "treasury.treasury")
const updat = blueprint.validators.find((v) => v.title === "updater.updater")

const minting_script: MintingPolicy = {
  type: "PlutusV2",
  script: applyDoubleCborEncoding(mint_script) //applyParamsToScript(
    //prop_mint?.compiledCode,
    //[genesis_utxo]),
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

const datum = Data.from(utxo.datum);
const count = datum.fields[0]

const unit = policyId + fromText("proposal_" + String(count))
const res_unit = policyId + fromText("proposal_" + String(count) + "_R")
const claim_unit = policyId + fromText("proposal_" + String(count) + "_Claim")

const prop_datum = Data.to(new Constr(0, [
  fromText("proposal_" + String(count)),
  fromText(" "),
  fromText(" "),
  fromText("INIT"),
  0n,
])) 

console.log(Data.from(prop_datum))

const nu_count = count + 1n 
const nu_datum = Data.to(new Constr(0, [nu_count]));

const mint_redeemer = Data.to(new Constr(0, []));

const mint_tx = await lucid
  .newTx()
  .readFrom([utxo])
  .mintAssets({ [unit]: 1n, [res_unit]: 1n, [claim_unit]: 1n}, mint_redeemer)
  .payToContract(updater_address, prop_datum, {unit: 1n} )
  .payToAddress(address, {[res_unit]: 1n})
  .payToAddress(address, {[claim_unit]: 1n})
  .payToContract(minting_address, nu_datum, {unArxh: 1n})
  .attachMintingPolicy(minting_script)
  .complete()

const mint_signedTx = await mint_tx.sign().complete();

const mint_txHash = await mint_signedTx.submit();
console.log(mint_txHash)

