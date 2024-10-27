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

const { validator, validatorHash, validatorAddress, outRef }: Genesis = JSON.parse(genesisFile);

const wallet = await lucid.selectWalletFromSeed(await Deno.readTextFile("./stuff/seed"));

const { paymentCredential: user_cred } = lucid.utils.getAddressDetails(
  await lucid.wallet.address()
);
const address = await lucid.wallet.address();
const utxos = await lucid.utxosAt(address);

const genesis_utxo = new Constr(0, [
  new Constr(0, [outRef.txHash]),
  BigInt(outRef.index),
]);

const unApxn = blueprint.validators.find((v) => v.title === "unapxn.unApxn");
const prop_mint = blueprint.validators.find((v) => v.title === "prop_mint.prop_mint");

const unApxn_script: MintingPolicy = {
  type: "PlutusV2",
  script: applyParamsToScript(
    unApxn?.compiledCode,
    [genesis_utxo],
  ),
};

const unApxn_addr = lucid.utils.validatorToAddress(unApxn_script)
const unApxn_pid = lucid.utils.mintingPolicyToId(unApxn_script)
const { paymentCredential: unApxn_cred } = lucid.utils.getAddressDetails(unApxn_addr)

const minting_script: MintingPolicy = {
  type: "PlutusV2",
  script: applyParamsToScript(
    prop_mint?.compiledCode,
    [unApxn_cred?.hash],
  ),
};

const proposal_addr = lucid.utils.validatorToAddress(minting_script)
const proposal_pid = lucid.utils.mintingPolicyToId(minting_script)
const { paymentCredential: proposal_cred } = lucid.utils.getAddressDetails(proposal_addr)


const unApxn_nft = unApxn_pid + fromText("unApxn")

const [utxo] = await lucid.utxosAtWithUnit(unApxn_addr, unApxn_nft)

let unApxn_datum = Data.from(utxo.datum!);

let count = unApxn_datum.fields[0];

const unit = proposal_pid + fromText("proposal_" + String(count))
const res_unit = proposal_pid + fromText("proposal_" + String(count) + "_R")
const claim_unit = proposal_pid + fromText("proposal_" + String(count) + "_Claim")

const proposal = prompt("proposal:");
const amount = prompt("amount:")

const prop_datum = Data.to(new Constr(0, [
  fromText(proposal),
  fromText("INIT"),
  BigInt(amount) * 1_000_000n
]));

const nu_datum = Data.to(new Constr(0, [
  count + 1n,
  unApxn_datum.fields[1],
  unApxn_datum.fields[2],
]));

const mint_redeemer = Data.to(new Constr(0, []));
const spend_redeemer = Data.to(new Constr(1, [new Constr(1, [])]));

console.log("minting redeemer:")
console.log(Data.from(mint_redeemer))
console.log("\nold unApxn datum:")
console.log(unApxn_datum)
console.log("\nnew unApxn datum:")
console.log(Data.from(nu_datum))
console.log("\nproposal datum:")
console.log(Data.from(prop_datum))
console.log("\naddress:")
console.log(address)


const mint_tx = await lucid
  .newTx()
  .readFrom([utxo])
  .attachMintingPolicy(minting_script)
  .collectFrom([utxo], spend_redeemer)
  .mintAssets({
    [unit]: 1n,
    [res_unit]: 1n,
    [claim_unit]: 1n,
  },
    mint_redeemer)
  .payToContract(unApxn_addr,
    {
      inline: nu_datum,
      scriptRef: unApxn_script
    },
    { [unApxn_nft]: 1n, lovelace: utxo.assets.lovelace })
  .payToContract(proposal_addr,
    { inline: Data.to(fromText("Banka")) },
    { lovelace: 200_000_000n })
  .payToContract(proposal_addr,
    { inline: prop_datum },
    { [unit]: 1n, })
  .payToAddress(address,
    {
      [res_unit]: 1n,
      [claim_unit]: 1n,
    })
  .complete()

const mint_signedTx = await mint_tx.sign().complete();

const mint_txHash = await mint_signedTx.submit();
console.log(mint_txHash)

