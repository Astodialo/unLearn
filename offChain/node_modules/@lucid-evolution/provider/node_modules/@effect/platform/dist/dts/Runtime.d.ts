import type * as Effect from "effect/Effect";
import * as Exit from "effect/Exit";
/**
 * @category model
 * @since 1.0.0
 */
export interface Teardown {
    <E, A>(exit: Exit.Exit<E, A>, onExit: (code: number) => void): void;
}
/**
 * @category teardown
 * @since 1.0.0
 */
export declare const defaultTeardown: Teardown;
/**
 * @category model
 * @since 1.0.0
 */
export interface RunMain {
    <E, A>(effect: Effect.Effect<A, E>, options?: {
        readonly disableErrorReporting?: boolean;
        readonly teardown?: Teardown;
    }): void;
}
//# sourceMappingURL=Runtime.d.ts.map