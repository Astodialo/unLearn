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

//const wallet = await lucid.selectWalletFromPrivateKey(await Deno.readTextFile("./stuff/key.sk"));
const wallet = await lucid.selectWalletFromSeed(await Deno.readTextFile("./stuff/seed"));

const { paymentCredential} = lucid.utils.getAddressDetails(
  await lucid.wallet.address()
);
const address = lucid.wallet.address();
const [utxo] = await lucid.wallet.getUtxos();

const genesis_utxo = new Constr(0, [
  new Constr(0, [utxo.txHash]),
  BigInt(utxo.outputIndex),
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

const UnArxh_schema = Data.Object({
  name: Data.Bytes(),
  count: Data.Integer(),
});

type UnArxh = Data.Static<typeof UnArxh_schema>;
const UnArxh = UnArxh_schema as unknown as UnArxh;

const policyId = lucid.utils.mintingPolicyToId(minting_script)

const unArxh = policyId + fromText("unArxh")

const genesis_redeemer = Data.to(new Constr(0, []));
const genesis_datum = Data.to(new Constr(0, [0n]));

console.log(Data.from(genesis_redeemer))
console.log(Data.from(genesis_datum))
console.log(minting_script)

const tx = await lucid
  .newTx()
  .collectFrom([utxo])
  .mintAssets({ [unArxh]: 1n }, genesis_redeemer,)
  .payToAddressWithData(minting_address, {inline: genesis_datum}, {[unArxh]: 1n, lovelace: 10000000n, })
  .attachMintingPolicy( minting_script,)
  .complete()

const signedTx = await tx.sign().complete();
const txHash = await signedTx.submit();

const genesis = "{\n  \"validator\": \"" + minting_script.script + "\"," + "\n"
              + "  \"validatorHash\": \"" +  lucid.utils.validatorToScriptHash(minting_script) + "\"," + "\n"
              + "  \"validatorAddress\": \"" + minting_address + "\"," + "\n"
              + "  \"outRef\": { \n"
              + "    \"txHash\": \"" + utxo.txHash + "\"," + "\n" 
              + "    \"index\": " + utxo.outputIndex + "\n" 
              + "  }\n"
              + "}"


await Deno.writeTextFileSync("./stuff/genesis.json", genesis); 

console.log(txHash);
