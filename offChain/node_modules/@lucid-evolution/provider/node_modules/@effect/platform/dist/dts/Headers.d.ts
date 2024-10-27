/**
 * @since 1.0.0
 */
import * as Schema from "@effect/schema/Schema";
import * as FiberRef from "effect/FiberRef";
import type * as Option from "effect/Option";
import * as Record from "effect/Record";
import * as Redacted from "effect/Redacted";
/**
 * @since 1.0.0
 * @category type ids
 */
export declare const HeadersTypeId: unique symbol;
/**
 * @since 1.0.0
 * @category type ids
 */
export type HeadersTypeId = typeof HeadersTypeId;
/**
 * @since 1.0.0
 * @category refinements
 */
export declare const isHeaders: (u: unknown) => u is Headers;
/**
 * @since 1.0.0
 * @category models
 */
export interface Headers {
    readonly [HeadersTypeId]: HeadersTypeId;
    readonly [key: string]: string;
}
/**
 * @since 1.0.0
 * @category schemas
 */
export declare const schemaFromSelf: Schema.Schema<Headers>;
/**
 * @since 1.0.0
 * @category schemas
 */
export declare const schema: Schema.Schema<Headers, Record.ReadonlyRecord<string, string | ReadonlyArray<string>>>;
/**
 * @since 1.0.0
 * @category models
 */
export type Input = Record.ReadonlyRecord<string, string | ReadonlyArray<string> | undefined> | Iterable<readonly [string, string]>;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const empty: Headers;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const fromInput: (input?: Input) => Headers;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const unsafeFromRecord: (input: Record.ReadonlyRecord<string, string>) => Headers;
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const has: {
    (key: string): (self: Headers) => boolean;
    (self: Headers, key: string): boolean;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const get: {
    (key: string): (self: Headers) => Option.Option<string>;
    (self: Headers, key: string): Option.Option<string>;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const set: {
    (key: string, value: string): (self: Headers) => Headers;
    (self: Headers, key: string, value: string): Headers;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const setAll: {
    (headers: Input): (self: Headers) => Headers;
    (self: Headers, headers: Input): Headers;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const merge: {
    (headers: Headers): (self: Headers) => Headers;
    (self: Headers, headers: Headers): Headers;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const remove: {
    (key: string): (self: Headers) => Headers;
    (self: Headers, key: string): Headers;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const redact: {
    (key: string | RegExp | ReadonlyArray<string | RegExp>): (self: Headers) => Record<string, string | Redacted.Redacted>;
    (self: Headers, key: string | RegExp | ReadonlyArray<string | RegExp>): Record<string, string | Redacted.Redacted>;
};
/**
 * @since 1.0.0
 * @category fiber refs
 */
export declare const currentRedactedNames: FiberRef.FiberRef<readonly (string | RegExp)[]>;
//# sourceMappingURL=Headers.d.ts.map