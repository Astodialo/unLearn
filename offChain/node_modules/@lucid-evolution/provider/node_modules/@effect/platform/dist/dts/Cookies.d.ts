/**
 * @since 1.0.0
 */
import * as Duration from "effect/Duration";
import * as Either from "effect/Either";
import * as Inspectable from "effect/Inspectable";
import { type Pipeable } from "effect/Pipeable";
import * as Record from "effect/Record";
import type * as Types from "effect/Types";
/**
 * @since 1.0.0
 * @category type ids
 */
export declare const TypeId: unique symbol;
/**
 * @since 1.0.0
 * @category type ids
 */
export type TypeId = typeof TypeId;
/**
 * @since 1.0.0
 * @category refinements
 */
export declare const isCookies: (u: unknown) => u is Cookies;
/**
 * @since 1.0.0
 * @category models
 */
export interface Cookies extends Pipeable, Inspectable.Inspectable {
    readonly [TypeId]: TypeId;
    readonly cookies: Record.ReadonlyRecord<string, Cookie>;
}
/**
 * @since 1.0.0
 * @category type ids
 */
export declare const CookieTypeId: unique symbol;
/**
 * @since 1.0.0
 * @category type ids
 */
export type CookieTypeId = typeof CookieTypeId;
/**
 * @since 1.0.0
 * @category cookie
 */
export interface Cookie extends Inspectable.Inspectable {
    readonly [CookieTypeId]: CookieTypeId;
    readonly name: string;
    readonly value: string;
    readonly valueEncoded: string;
    readonly options?: {
        readonly domain?: string | undefined;
        readonly expires?: Date | undefined;
        readonly maxAge?: Duration.DurationInput | undefined;
        readonly path?: string | undefined;
        readonly priority?: "low" | "medium" | "high" | undefined;
        readonly httpOnly?: boolean | undefined;
        readonly secure?: boolean | undefined;
        readonly partitioned?: boolean | undefined;
        readonly sameSite?: "lax" | "strict" | "none" | undefined;
    } | undefined;
}
/**
 * @since 1.0.0
 * @category type ids
 */
export declare const ErrorTypeId: unique symbol;
/**
 * @since 1.0.0
 * @category type ids
 */
export type ErrorTypeId = typeof ErrorTypeId;
declare const CookiesError_base: new <A extends Record<string, any>>(args: Types.Simplify<A>) => import("effect/Cause").YieldableError & Record<typeof ErrorTypeId, typeof ErrorTypeId> & {
    readonly _tag: "CookieError";
} & Readonly<A>;
/**
 * @since 1.0.0
 * @category errors
 */
export declare class CookiesError extends CookiesError_base<{
    readonly reason: "InvalidName" | "InvalidValue" | "InvalidDomain" | "InvalidPath" | "InfinityMaxAge";
}> {
    get message(): "InvalidName" | "InvalidValue" | "InvalidDomain" | "InvalidPath" | "InfinityMaxAge";
}
/**
 * Create a Cookies object from an Iterable
 *
 * @since 1.0.0
 * @category constructors
 */
export declare const fromReadonlyRecord: (cookies: Record.ReadonlyRecord<string, Cookie>) => Cookies;
/**
 * Create a Cookies object from an Iterable
 *
 * @since 1.0.0
 * @category constructors
 */
export declare const fromIterable: (cookies: Iterable<Cookie>) => Cookies;
/**
 * Create a Cookies object from a set of Set-Cookie headers
 *
 * @since 1.0.0
 * @category constructors
 */
export declare const fromSetCookie: (headers: Iterable<string> | string) => Cookies;
/**
 * An empty Cookies object
 *
 * @since 1.0.0
 * @category constructors
 */
export declare const empty: Cookies;
/**
 * @since 1.0.0
 * @category refinements
 */
export declare const isEmpty: (self: Cookies) => boolean;
/**
 * Create a new cookie
 *
 * @since 1.0.0
 * @category constructors
 */
export declare function makeCookie(name: string, value: string, options?: Cookie["options"] | undefined): Either.Either<Cookie, CookiesError>;
/**
 * Create a new cookie, throwing an error if invalid
 *
 * @since 1.0.0
 * @category constructors
 */
export declare const unsafeMakeCookie: (name: string, value: string, options?: Cookie["options"] | undefined) => Cookie;
/**
 * Add a cookie to a Cookies object
 *
 * @since 1.0.0
 * @category combinators
 */
export declare const setCookie: {
    (cookie: Cookie): (self: Cookies) => Cookies;
    (self: Cookies, cookie: Cookie): Cookies;
};
/**
 * Add multiple cookies to a Cookies object
 *
 * @since 1.0.0
 * @category combinators
 */
export declare const setAllCookie: {
    (cookies: Iterable<Cookie>): (self: Cookies) => Cookies;
    (self: Cookies, cookies: Iterable<Cookie>): Cookies;
};
/**
 * Combine two Cookies objects, removing duplicates from the first
 *
 * @since 1.0.0
 * @category combinators
 */
export declare const merge: {
    (that: Cookies): (self: Cookies) => Cookies;
    (self: Cookies, that: Cookies): Cookies;
};
/**
 * Remove a cookie by name
 *
 * @since 1.0.0
 * @category combinators
 */
export declare const remove: {
    (name: string): (self: Cookies) => Cookies;
    (self: Cookies, name: string): Cookies;
};
/**
 * Add a cookie to a Cookies object
 *
 * @since 1.0.0
 * @category combinators
 */
export declare const set: {
    (name: string, value: string, options?: Cookie["options"]): (self: Cookies) => Either.Either<Cookies, CookiesError>;
    (self: Cookies, name: string, value: string, options?: Cookie["options"]): Either.Either<Cookies, CookiesError>;
};
/**
 * Add a cookie to a Cookies object
 *
 * @since 1.0.0
 * @category combinators
 */
export declare const unsafeSet: {
    (name: string, value: string, options?: Cookie["options"]): (self: Cookies) => Cookies;
    (self: Cookies, name: string, value: string, options?: Cookie["options"]): Cookies;
};
/**
 * Add multiple cookies to a Cookies object
 *
 * @since 1.0.0
 * @category combinators
 */
export declare const setAll: {
    (cookies: Iterable<readonly [name: string, value: string, options?: Cookie["options"]]>): (self: Cookies) => Either.Either<Cookies, CookiesError>;
    (self: Cookies, cookies: Iterable<readonly [name: string, value: string, options?: Cookie["options"]]>): Either.Either<Cookies, CookiesError>;
};
/**
 * Add multiple cookies to a Cookies object, throwing an error if invalid
 *
 * @since 1.0.0
 * @category combinators
 */
export declare const unsafeSetAll: {
    (cookies: Iterable<readonly [name: string, value: string, options?: Cookie["options"]]>): (self: Cookies) => Cookies;
    (self: Cookies, cookies: Iterable<readonly [name: string, value: string, options?: Cookie["options"]]>): Cookies;
};
/**
 * Serialize a cookie into a string
 *
 * Adapted from https://github.com/fastify/fastify-cookie under MIT License
 *
 * @since 1.0.0
 * @category encoding
 */
export declare function serializeCookie(self: Cookie): string;
/**
 * Serialize a Cookies object into a Cookie header
 *
 * @since 1.0.0
 * @category encoding
 */
export declare const toCookieHeader: (self: Cookies) => string;
/**
 * To record
 *
 * @since 1.0.0
 * @category encoding
 */
export declare const toRecord: (self: Cookies) => Record<string, string>;
/**
 * Serialize a Cookies object into Headers object containing one or more Set-Cookie headers
 *
 * @since 1.0.0
 * @category encoding
 */
export declare const toSetCookieHeaders: (self: Cookies) => Array<string>;
/**
 * Parse a cookie header into a record of key-value pairs
 *
 * Adapted from https://github.com/fastify/fastify-cookie under MIT License
 *
 * @since 1.0.0
 * @category decoding
 */
export declare function parseHeader(header: string): Record<string, string>;
export {};
//# sourceMappingURL=Cookies.d.ts.map