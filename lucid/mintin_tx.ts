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

const genesis_utxo = fromText("fc1b70ba8279ef492a6ef4411750c8b2a368fcef3f86224ce10a680b980ad630#0") 


const minting_script: MintingPolicy = {
  type: "PlutusV2",
  script: applyParamsToScript(
    blueprint.validators[0].compiledCode,
    [genesis_utxo],
  ),
}; 

const treasury_script: SpendingValidator = {
  type: "PlutusV2",
  script: blueprint.validators[1].compiledCode,
};

const updater_script: SpendingValidator = { 
  type: "PlutusV2",
  script: blueprint.validators[2].compiledCode,
};

const minting_address = lucid.utils.validatorToAddress(minting_script)
const treasury_address = lucid.utils.validatorToAddress(treasury_script)
const updater_address = lucid.utils.validatorToAddress(updater_script)

const UnArxh_schema = Data.Object({
  name: Data.Bytes(),
  count: Data.Integer(),
});

type UnArxh = Data.Static<typeof UnArxh_schema>;
const UnArxh = UnArxh_schema as unknown as UnArxh;

const Proposal_schema = Data.Object({
  name: Data.Bytes(),
  proposal: Data.Bytes(),
  results: Data.Bytes(), 
  state: Data.Bytes(),
  amount: Data.Integer(),
});


type Proposal = Data.Static<typeof Proposal_schema>;
const Proposal = Proposal_schema as unknown as Proposal;

const policyId = lucid.utils.mintingPolicyToId(minting_script)

const unArxh = policyId + fromText("unArxh")
console.log(unArxh)
export async function mint_proposal(prop: string, amt: bigint, action: string ): Promise<TxHash> {
  const prop_datum: Proposal = {
    name: name + String(count),
    proposal: prop,
    results: fromText(""),
    state: fromText("INIT"),
    amount: amt,
  }; 
  
  if (action == "genesis"){
    const redeemer = Data.to(new Constr(1, []))

    const tx = await lucid
      .newTx()
      .mintAssets({ [unArxh]: 1n })
      .attachMintingPolicy(minting_script)
      .complete()

    const signedTx = await tx.sign().complet();
    const txHash = await signedTx.submit();
    return txHash;
  } 

  const [utxo] = await lucid.utxosAtWithUnit(minting_address, unArxh) 
  const {name, count}: UnArxh = await lucid.datumOf(utxo) 

  const unit = policyId + name + String(count)

  const nu_count = count + 1n 
  const nu_datum = {name, count: nu_count}

  const redeemer = Data.to(new Constr(0, []))

  const tx = await lucid
    .newTx()
    .mintAssets({ [unit]: 1n, [unit + fromText("_R")]: 1n, [unit + fromText("_Claim")]: 1n}, redeemer)
    .payToContract(updater_address, Data.to(prop_datum, Proposal), {unit: 1n} )
    .payToAddress(address, {[unit + fromText("_R")]: 1n})
    .payToAddress(address, {[unit + fromText("_Claim")]: 1n})
    .payToContract(minting_address, Data.to(nu_datum, UnArxh), {unArxh: 1n})
    .attachMintingPolicy(minting_script)
    .complete()

  const signedTx = await tx.sign().complete();

  const txHash = await signedTx.submit();

  return txHash;

}


