import type { UTxO } from "@meshsdk/core";
import axios from "axios";

const instance = axios.create({
  baseURL: `/api/`,
  withCredentials: true,
});

export async function post(route: string, body = {}) {
  return await instance
    .post(`${route}`, body)
    .then(({ data }) => {
      return data;
    })
    .catch((error) => {
      throw error;
    });
}

export async function createTransaction(
  recipientAddress: string,
  utxos: UTxO[],
  input: String 
) {
  return await post(`create-mining-transaction`, { recipientAddress, utxos, input });
}

export async function signTransaction(
  signedTx: string,
  originalMetadata: string
) {
  return await post(`sign-transaction`, {
    signedTx,
    originalMetadata,
  });
}

export async function creatUpdateTx(
  recipientAddress: String,
  utxos: UTxO[],
  assName: String
) {
  return await post(`create-update-tx`, { recipientAddress, utxos, assName});
}

export async function signiUpdateTx(
  signedTx: string
) {
  return await post(`sign-transaction`, {
    signedTx
  });
}
