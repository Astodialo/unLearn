import { Lucid } from "https://deno.land/x/lucid@0.8.8/mod.ts";
 
const lucid = await Lucid.new(undefined, "Preprod");
 
const seed = lucid.utils.generateSeedPhrase();
await Deno.writeTextFile("stuff/seed", seed);

const address = await lucid
  .selectWalletFromSeed(seed)
  .wallet.address();

await Deno.writeTextFile("stuff/key.addr", address);
