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
  OutputData,
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

//const wallet = await lucid.selectWalletFromPrivateKey(await Deno.readTextFile("./stuff/key.sk"));
const wallet = await lucid.selectWalletFromSeed(await Deno.readTextFile("./stuff/seed"));

const { paymentCredential: user_cred } = lucid.utils.getAddressDetails(
  await lucid.wallet.address()
);


const address = await lucid.wallet.address();
const utxos = await lucid.wallet.getUtxos();

const genesis_utxo = new Constr(0, [
  new Constr(0, [utxos[0].txHash]),
  BigInt(utxos[0].outputIndex),
]);

const prop_mint = blueprint.validators.find((v) => v.title === "prop_mint.prop_mint");
const unApxn = blueprint.validators.find((v) => v.title === "unapxn.unApxn");


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
    [unApxn_cred!.hash],
  ),
};

const minting_address = lucid.utils.validatorToAddress(minting_script)
const proposal_pid = lucid.utils.mintingPolicyToId(minting_script)
const { paymentCredential: minting_cred } = lucid.utils.getAddressDetails(minting_address)

const unApxn_nft = unApxn_pid + fromText("unApxn")

const genesis_redeemer = Data.to(new Constr(0, []));
const genesis_datum = Data.to(new Constr(0, [
  0n,
  minting_cred?.hash,
  user_cred?.hash
]));

console.log(minting_cred?.hash)
console.log(user_cred?.hash)
console.log(Data.from(genesis_redeemer))
console.log(Data.from(genesis_datum))
console.log(minting_script)

const tx = await lucid
  .newTx()
  .collectFrom([utxos[0]],)
  .mintAssets({ [unApxn_nft]: 1n }, genesis_redeemer,)
  .payToAddressWithData(unApxn_addr, { inline: genesis_datum, scriptRef: unApxn_script }, { [unApxn_nft]: 1n, lovelace: 10_000_000n, })
  .attachMintingPolicy(unApxn_script,)
  .complete({ change: { address: address } })

const signedTx = await tx.sign().complete();
const txHash = await signedTx.submit();

const genesis = "{\n  \"validator\": \"" + minting_script.script + "\"," + "\n"
  + "  \"validatorHash\": \"" + lucid.utils.validatorToScriptHash(minting_script) + "\"," + "\n"
  + "  \"validatorAddress\": \"" + minting_address + "\"," + "\n"
  + "  \"outRef\": { \n"
  + "    \"txHash\": \"" + utxos[0].txHash + "\"," + "\n"
  + "    \"index\": " + utxos[0].outputIndex + "\n"
  + "  }\n"
  + "}"


await Deno.writeTextFileSync("./stuff/genesis.json", genesis);
console.log(txHash);
