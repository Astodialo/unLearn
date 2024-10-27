import * as Schema from "@effect/schema/Schema";
import * as Context from "effect/Context";
import * as Effect from "effect/Effect";
/**
 * @since 1.0.0
 * @category models
 */
export interface CollectorService {
    readonly addAll: (_: Iterable<globalThis.Transferable>) => Effect.Effect<void>;
    readonly unsafeAddAll: (_: Iterable<globalThis.Transferable>) => void;
    readonly read: Effect.Effect<ReadonlyArray<globalThis.Transferable>>;
    readonly unsafeRead: () => ReadonlyArray<globalThis.Transferable>;
    readonly unsafeClear: () => void;
    readonly clear: Effect.Effect<void>;
}
declare const Collector_base: Context.TagClass<Collector, "@effect/platform/Transferable/Collector", CollectorService>;
/**
 * @since 1.0.0
 * @category tags
 */
export declare class Collector extends Collector_base {
}
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const unsafeMakeCollector: () => CollectorService;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const makeCollector: Effect.Effect<CollectorService>;
/**
 * @since 1.0.0
 * @category accessors
 */
export declare const addAll: (tranferables: Iterable<globalThis.Transferable>) => Effect.Effect<void>;
/**
 * @since 1.0.0
 * @category schema
 */
export declare const schema: {
    <I>(f: (_: I) => Iterable<globalThis.Transferable>): <A, R>(self: Schema.Schema<A, I, R>) => Schema.Schema<A, I, R>;
    <A, I, R>(self: Schema.Schema<A, I, R>, f: (_: I) => Iterable<globalThis.Transferable>): Schema.Schema<A, I, R>;
};
/**
 * @since 1.0.0
 * @category schema
 */
export declare const ImageData: Schema.Schema<ImageData>;
/**
 * @since 1.0.0
 * @category schema
 */
export declare const MessagePort: Schema.Schema<MessagePort>;
/**
 * @since 1.0.0
 * @category schema
 */
export declare const Uint8Array: Schema.Schema<Uint8Array>;
export {};
//# sourceMappingURL=Transferable.d.ts.map