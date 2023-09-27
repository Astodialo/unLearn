import { 
  Lucid,
  Kupmios,
  MintingPolicy,
  SpendingValidator, 
  fromText, 
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

const wallet = await lucid.selectWalletFromPrivateKey(await Deno.readTextFile("./key.sk"));

const { paymentCredential} = lucid.utils.getAddressDetails(
  await lucid.wallet.address()
);
const address = lucid.wallet.address();

const Out_Ref_Schema = Data.Object({
  transaction_id: Data.Bytes(),
  output_index: Data.Integer(),
});

type Out_Ref = Data.Static<typeof Out_Ref_Schema>;
const Out_Ref = Out_Ref_Schema as unknown as Out_Ref;

const genesis_utxo: Out_Ref = Data.to({ 
  transaction_id: fromText("fc1b70ba8279ef492a6ef4411750c8b2a368fcef3f86224ce10a680b980ad630#0"),
  output_index: 0n 
}, Out_Ref,);


const minting_script: MintingPolicy = {
  type: "PlutusV2",
  script: applyParamsToScript(
    blueprint.validators[0].compiledCode,
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

const Action = Data.Enum([
  Data.Literal("Mintin"),
  Data.Literal("Genesis"),
]);

type Action = Data.Static<typeof Action>;

const genesis_datum: UnArxh = {name: fromText("proposal"), count: 0n } 

const redeemer: Action = Data.to<Action>("Genesis", Action)

console.log(redeemer)

const tx = await lucid
  .newTx()
  .mintAssets({ [unArxh]: 1n }, redeemer)
  .payToAddressWithData(minting_address, {inline: Data.to(genesis_datum, UnArxh)}, {[unArxh]: 1n, lovelace: 10000000n, })
  .attachMintingPolicy(minting_script)
  .complete()

const signedTx = await tx.sign().complete();
const txHash = await signedTx.submit();
console.log(txHash);
