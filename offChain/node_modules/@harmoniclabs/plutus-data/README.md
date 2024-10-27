# @harmoniclabs/plutus-data

Typescript definition of the generic `Data` type for plutus applicaitons

```ts
type Data 
    = DataConstr
    | DataMap<Data, Data>
    | DataList
    | DataI
    | DataB;
```

## install

```bash
npm install @harmoniclabs/plutus-data
```

## exported functions

```ts
function hashData(data: Data): Uint8Array;
function dataToCborObj(data: Data): CborObj;
function dataToCbor(data: Data): CborString;
function dataFromCborObj(cborObj: CborObj): Data;
function dataFromCbor(cbor: CanBeCborString): Data;
```