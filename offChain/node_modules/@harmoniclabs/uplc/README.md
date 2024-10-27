# @harmoniclabs/uplc

Typescript/Javascript representation of UPLC (Untyped PLutus Core).

## Install

```bash
npm install @harmoniclabs/uplc
```

## Getting started


### parse serialized UPLC
parse and print uplc form flat hex ([`@harmoniclabs/uint8array-utils`](https://github.com/HarmonicLabs/uint8array-utils) works in every js runtime)
```ts
import { fromHex } from "@harmoniclabs/uint8array-utils";
import { parseUPLC, prettyUPLC } from "@harmoniclabs/uplc";

const serialized: Uint8Array = fromHex( "0100003233700900219b8248050005200801" );

const program = parseUPLC( serialized, "flat" );

console.log(
    prettyUPLC(
        program.body, // UPLCTerm 
        4 // indentation spaces
    )
);
/*
expected output:

[
    (lam a 
        [
            [
                (builtin addInteger) 
                (con integer 2)
            ] 
            [
                [
                    (builtin multiplyInteger) 
                    (con integer 10)
                ] 
                a
            ]
        ]
    ) 
    (con integer 4)
]
    
*/
```

### parse textual UPLC

```ts
import { parseUPLCText, prettyUPLC } from "@harmoniclabs/uplc";

const uplc_source = `
[
    (lam a 
        [
            [
                (builtin addInteger) 
                (con integer 2)
            ] 
            [
                [
                    (builtin multiplyInteger) 
                    (con integer 10)
                ] 
                a
            ]
        ]
    ) 
    (con integer 4)
]`;

const uplc = parseUPLCText( uplc_source );

// expected output: true
console.log(
    prettyUPLC( uplc, 4 ) === uplc_source
);
```

### compile UPLC to flat bytes

```ts
import { toHex } from "@harmoniclabs/uint8array-utils";
import { Application, Builtin, UPLCConst, compileUPLC, UPLCProgram } from "@harmoniclabs/uplc";

const body = new Application(
    new Application(
        Builtin.addInteger,
        UPLCConst.int( 2 )
    ),
    UPLCConst.int( 2 )
);

const compiled = compileUPLC(
    new UPLCProgram(
        [1,0,0],
        body
    )
).toBuffer().buffer;

console.log( toHex( compiled ) );// expected output: "01000033700900224009"
```