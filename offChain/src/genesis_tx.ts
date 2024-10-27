import {
  Lucid,
  Kupmios,
  MintingPolicy,
  SpendingValidator,
  fromText,
  applyDoubleCborEncoding,
  Data,
  TxHash,
  Constr,
  OutputData,
  fromHex,
  toHex,
  toLabel,
  OutRef,
  Blockfrost,
  applyParamsToScript,
  paymentCredentialOf,
  scriptHashToCredential,
  mintingPolicyToId,
  credentialToAddress,
} from "@lucid-evolution/lucid";
import * as fs from 'fs';
import blueprint from "../../onChain/plutus.json" assert { type: "json"};

const lucid = await Lucid(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0/",
    "preprodGoGVMAiqZOuhaZiKG2MSm16jW11NtaY3"
  ),
  "Preprod",
);

//const wallet = await lucid.selectWalletFromPrivateKey(await Deno.readTextFile("./stuff/key.sk"));
const seed_phrase = fs.readFileSync('./stuff/seed', 'utf8');
const wallet = await lucid.selectWallet.fromSeed(seed_phrase);

const user_cred = paymentCredentialOf(
  await lucid.wallet().address()
);


const address = await lucid.wallet().address();
const utxos = await lucid.wallet().getUtxos();

const OutputReferenceSchema = Data.Object({
  txHash: Data.Bytes({ minLength: 32, maxLength: 32 }),
  outputIndex: Data.Integer(),
});
type OutputReference = Data.Static<typeof OutputReferenceSchema>;

const OutputReference =
  OutputReferenceSchema as unknown as OutputReference;

const params: OutputReference = {
  txHash: utxos[0].txHash,
  outputIndex: BigInt(utxos[0].outputIndex),
};

const genesis_utxo: Data = Data.from(Data.to(params, OutputReference));

const prop_mint = blueprint.validators.find((v) => v.title === "prop_mint.prop_mint");
const unApxn = blueprint.validators.find((v) => v.title === "unapxn.unApxn");

const unApxn_script: MintingPolicy = {
  type: "PlutusV2",
  script: applyParamsToScript(
    (unApxn.compiledCode),
    [genesis_utxo],
  ),
};

const unApxn_cred = scriptHashToCredential(unApxn_script.script);
const unArxn_addr = credentialToAddress("Preprod", unApxn_cred);
const unApxn_pid = mintingPolicyToId(unApxn_script);

const minting_script: MintingPolicy = {
  type: "PlutusV2",
  script: applyParamsToScript(
    applyDoubleCborEncoding(prop_mint.hash),
    [unApxn_cred!.hash],
  ),
};

const minting_cred = scriptHashToCredential(minting_script.script)
const minting_addr = credentialToAddress("Preprod", minting_cred)
const proposal_pid = mintingPolicyToId(minting_script)

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

const tx = await lucid
  .newTx()
  .collectFrom([utxos[0]],)
  .mintAssets({ [unApxn_nft]: 1n }, genesis_redeemer,)
  .pay.ToAddressWithData(unArxn_addr, { kind: "inline", value: genesis_datum }, { [unApxn_nft]: 1n, lovelace: 10_000_000n, }, unApxn_script)
  .attach.MintingPolicy(unApxn_script)
  .complete()

const signedTx = await tx.sign.withWallet().complete();
const txHash = await signedTx.submit();

const genesis = "{\n  \"validator\": \"" + minting_script.script + "\"," + "\n"
  + "  \"validatorHash\": \"" + minting_script.script + "\"," + "\n"
  + "  \"validatorAddress\": \"" + minting_addr + "\"," + "\n"
  + "  \"outRef\": { \n"
  + "    \"txHash\": \"" + utxos[0].txHash + "\"," + "\n"
  + "    \"index\": " + utxos[0].outputIndex + "\n"
  + "  }\n"
  + "}"

console.log(txHash);
