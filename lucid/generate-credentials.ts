import { Lucid } from "https://deno.land/x/lucid@0.8.8/mod.ts";
 
const lucid = await Lucid.new(undefined, "Preprod");
 
const privateKey = lucid.utils.generatePrivateKey();
await Deno.writeTextFile("stuff/key.sk", privateKey);
 
const address = await lucid
  .selectWalletFromPrivateKey(privateKey)
  .wallet.address();
await Deno.writeTextFile("stuff/key.addr", address);
