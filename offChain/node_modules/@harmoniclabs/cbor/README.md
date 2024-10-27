# CBOR (Concise Binary Object Representation)

[Specification](https://datatracker.ietf.org/doc/html/rfc7049)

## Installation

```bash
npm install @harmoniclabs/cbor
```

## Getting started

```ts
import { Cbor, CborText } from "@harmoniclabs/cbor";

const input = "6B68656C6C6F20776F726C64";

const cborObj = Cbor.parse( input );

if( cborObj instanceof CborText )
{
    console.log( cborObj.text ) // prints "hello world"
}

const encoded = Cbor.encode( cborObj ).toString();

console.log( encoded ) // prints "6b68656c6c6f20776f726c64" (lower case hex)

```

## How to use

### `Cbor`

to parse/encode CBOR you can use the `Cbor` static class.

```ts
export declare class Cbor {
    private constructor();
    static encode(cborObj: CborObj): CborString;
    static parse(cbor: CborString | Uint8Array | string): CborObj;
}
```

`parse` allows you to go from a thing that can represent CBOR (a `CborString` object is always valid) to a `CborObj`

`encode` takes a `CborObj` and returns a `CborString`.

### `CborString`

is an object that represents a valid CBOR string;

extending a [`ByteString`](https://github.com/HarmonicLabs/bytestring) includes:

- `toString` method that returns a lower case hex string.
- `toBuffer` method that returns the `Uint8Array` representation of the string.

```ts
class CborString {
    toString(): string
    toBuffer(): Uint8Array
}
```

### `CborObj`

result of `Cbor.parse`; represents a generic CBOR tag corresponding to one of the possible Major types.

```ts
type CborObj
    = CborNegInt
    | CborUInt
    | CborBytes
    | CborText
    | CborArray
    | CborMap
    | CborTag
    | CborSimple;
```