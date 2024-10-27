# @harmoniclabs/bigint-utils

## exported functions

```ts
function abs(n: bigint): bigint;
function random(): bigint;
function max(...nums: bigint[]): bigint;
function min(...nums: bigint[]): bigint;
function log2(num: bigint): bigint;
function bigintFromBufferLE(buffer: Uint8Array): bigint;
function bigintFromBuffer(buffer: Uint8Array): bigint;
function bigintToBuffer(bigint: bigint, nBytes?: number | undefined): Uint8Array;
```