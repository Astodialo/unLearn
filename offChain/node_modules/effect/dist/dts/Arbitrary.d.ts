/**
 * @since 3.10.0
 */
import * as FastCheck from "./FastCheck.js";
import type * as Schema from "./Schema.js";
/**
 * @category model
 * @since 3.10.0
 */
export interface LazyArbitrary<A> {
    (fc: typeof FastCheck): FastCheck.Arbitrary<A>;
}
/**
 * @category annotations
 * @since 3.10.0
 */
export interface ArbitraryGenerationContext {
    readonly depthIdentifier?: string;
    readonly maxDepth: number;
}
/**
 * @category annotations
 * @since 3.10.0
 */
export type ArbitraryAnnotation<A, TypeParameters extends ReadonlyArray<any> = readonly []> = (...arbitraries: [
    ...{
        readonly [K in keyof TypeParameters]: LazyArbitrary<TypeParameters[K]>;
    },
    ctx: ArbitraryGenerationContext
]) => LazyArbitrary<A>;
/**
 * Returns a LazyArbitrary for the `A` type of the provided schema.
 *
 * @category arbitrary
 * @since 3.10.0
 */
export declare const makeLazy: <A, I, R>(schema: Schema.Schema<A, I, R>) => LazyArbitrary<A>;
/**
 * Returns a fast-check Arbitrary for the `A` type of the provided schema.
 *
 * @category arbitrary
 * @since 3.10.0
 */
export declare const make: <A, I, R>(schema: Schema.Schema<A, I, R>) => FastCheck.Arbitrary<A>;
//# sourceMappingURL=Arbitrary.d.ts.map