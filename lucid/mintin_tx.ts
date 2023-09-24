import { Lucid, Kupmios, MintingPolicy, SpendingValidator, fromText, Data, TxHash, sign, Constr } from "lucid-cardano";
import { prop_mint_code, updater_code, voting_code } from "../lucid/params"

const lucid = await Lucid.new(
  new Kupmios(
    "http://localhost:1442",
    "ws://localhost:1337",
  ),
  "Preprod",
);

const api = await window.cardano.nami.enable();
lucid.selectWallet(api)

const { paymentCredential} = lucid.utils.getAddressDetails(
  await lucid.wallet.address()
);
const address = await lucid.wallet.address();

const minting_script: MintingPolicy = {
  type: "PlutusV2",
  script: prop_mint_code,
}
const updater_script: SpendingValidator = {
  type: "PlutusV2",
  script: updater_code,
}
const voting_script: SpendingValidator = {
  type: "PlutusV2",
  script: voting_code,
}

const minting_address = lucid.utils.validatorToAddress(minting_script)
const updater_address = lucid.utils.validatorToAddress(updater_script)
const voting_address = lucid.utils.validatorToAddress(voting_script)


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
const [utxo] = await lucid.utxosAtWithUnit(minting_address, unArxh) 

const {name, count}: UnArxh = await lucid.datumOf(utxo) 

const unit = policyId + name + String(count)

const nu_count = count + 1n 
const nu_datum = {name, count: nu_count}

export async function mint_proposal(prop: string, amt: bigint): Promise<TxHash> {
  const prop_datum: Proposal = {
    name: name + String(count),
    proposal: prop,
    results: fromText(""),
    state: fromText("INIT"),
    amount: amt,
  }; 

  const redeemer = Data.to(new Constr(0, []))

  const tx = await lucid
    .newTx()
    .mintAssets({ [unit]: 1n, [unit + fromText("_R")]: 1n, [unit + fromText("_Claim")]: 1n}, redeemer)
    .payToContract(updater_address, Data.to(prop_datum, Proposal), {unit: 1n} )
    .payToAddress(voting_address, {[unit + fromText("_R")]: 1n})
    .payToAddress(address, {[unit + fromText("_Claim")]: 1n})
    .payToContract(minting_address, Data.to(nu_datum, UnArxh), {unArxh: 1n})
    .attachMintingPolicy(minting_script)

  const signedTx = await tx.sign().complete();

  const txHash = await signedTx.submit();

  return txHash;

}


