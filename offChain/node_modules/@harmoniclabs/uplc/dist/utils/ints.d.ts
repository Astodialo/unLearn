export type CanBeUInteger = bigint | number;
export declare function canBeUInteger(something: any): something is (number | bigint);
export declare function forceBigUInt(toForce: CanBeUInteger): bigint;
export declare function unsafeForceUInt(toForce: CanBeUInteger): number;
