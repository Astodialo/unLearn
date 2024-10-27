"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.unsafeForceUInt = exports.forceBigUInt = exports.canBeUInteger = void 0;
function canBeUInteger(something) {
    return ((typeof something === "bigint" && something >= BigInt(0)) ||
        (typeof something === "number" && something === Math.round(Math.abs(something))));
}
exports.canBeUInteger = canBeUInteger;
function forceBigUInt(toForce) {
    if (!canBeUInteger(toForce)) {
        // console.error( toForce );
        throw new Error("trying to convert an integer to an unsigned Integer, the number was negative");
    }
    return BigInt(toForce);
}
exports.forceBigUInt = forceBigUInt;
;
function unsafeForceUInt(toForce) {
    if (!canBeUInteger(toForce)) {
        // console.error( toForce );
        throw new Error("trying to convert an integer to an unsigned Integer, the number was negative");
    }
    return Number(toForce);
}
exports.unsafeForceUInt = unsafeForceUInt;
