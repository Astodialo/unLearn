import type { AssetMetadata } from "@meshsdk/core";

export const costLovelace = "10000000";

export const bankWalletAddress = `addr_test1qzmwuzc0qjenaljs2ytquyx8y8x02en3qxswlfcldwetaeuvldqg2n2p8y4kyjm8sqfyg0tpq9042atz0fr8c3grjmysm5e6yx`;

export var idArray = Array.from(
  {length: 100},
  (_, index) => 100 - index
);

export const idanikoMetadata = {
  0: {
    name: "idaniko",
    ena: "leuteria.",
  }
};


export const assetsMetadata: { [id: string]: AssetMetadata } = {
  "0": {
    name: "Proposal 00",
  },
  "1": {
    name: "Proposal 01",
  },
  "2": {
    name: "Proposal 02",
  },
  "3": {
    name: "Proposal 03",
  },
  "4": {
    name: "Proposal 04",
  },
  "5": {
    name: "Proposal 05",
  },
  "6": {
    name: "Proposal 06",
  },
  "7": {
    name: "Proposal 07",
  },
  "8": {
    name: "Proposal 08",
  },
  "9": {
    name: "Proposal 09",
  },
};
