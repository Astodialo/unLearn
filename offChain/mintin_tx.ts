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
const address = await lucid.wallet.address();

const genesis_utxo = new Constr(0, [
  new Constr(0, [outRef.txHash]),
  BigInt(outRef.index),
]);

const prop_mint = blueprint.validators.find((v) => v.title === "proposal_mint.prop_mint");

const minting_script: MintingPolicy = {
  type: "PlutusV2",
  script: applyParamsToScript(
    prop_mint?.compiledCode,
    [genesis_utxo],
  ),
}; 

const minting_address = lucid.utils.validatorToAddress(minting_script)

const policyId = lucid.utils.mintingPolicyToId(minting_script)

const unArxh = policyId + fromText("unArxh")

const [utxo] = await lucid.utxosAtWithUnit(minting_address, unArxh) 

let unArxh_datum = Data.from(utxo.datum!);
 
let count = unArxh_datum.fields[0];

const unit = policyId + fromText("proposal_" + String(count))
const res_unit = policyId + fromText("proposal_" + String(count) + "_R")
const claim_unit = policyId + fromText("proposal_" + String(count) + "_Claim")

const proposal = prompt("proposal:");
const amount = prompt("amount:")

const prop_datum = Data.to(new Constr(0, [
  fromText(proposal),
  fromText("INIT"),
  BigInt(amount) * 1_000_000n
])); 

const nu_datum = Data.to(new Constr(0, [
  count + 1n,
  unArxh_datum.fields[1]   
]));

const mint_redeemer = Data.to(new Constr(1, []));
const spend_redeemer = Data.to(new Constr(1, [new Constr(1, [])]));

console.log("minting redeemer:")
console.log(Data.from(mint_redeemer))
console.log("\nold unArxh datum:")
console.log(unArxh_datum)
console.log("\nnew unArxh datum:") 
console.log(Data.from(nu_datum))
console.log("\nproposal datum:") 
console.log(Data.from(prop_datum))
console.log("\naddress:")
console.log(address)

const mint_tx = await lucid
  .newTx()
  .collectFrom([utxo], spend_redeemer)
  .mintAssets({ [unit]: 1n, [res_unit]: 1n, [claim_unit]: 1n,}, mint_redeemer)
  .payToAddressWithData(minting_address, {inline: nu_datum}, {[unArxh]: 1n, lovelace: utxo.assets.lovelace})
  .payToAddressWithData(minting_address, {inline: Data.to(fromText("Banka"))}, {lovelace:200_000_000n})
  .payToAddressWithData(minting_address, { inline: prop_datum}, {[unit]: 1n,} )
  .payToAddress(address, {[res_unit]: 1n,})
  .payToAddress(address, {[claim_unit]: 1n,})
  .attachMintingPolicy(minting_script)
  .complete({change: {address: address}})

const mint_signedTx = await mint_tx.sign().complete();

const mint_txHash = await mint_signedTx.submit();
console.log(mint_txHash)

