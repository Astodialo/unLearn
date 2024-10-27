/**
 * @since 1.0.0
 */
import type { ParseOptions } from "@effect/schema/AST";
import type * as Schema from "@effect/schema/Schema";
import type * as Effect from "effect/Effect";
import type { Inspectable } from "effect/Inspectable";
import type * as Option from "effect/Option";
import type { Scope } from "effect/Scope";
import type * as Stream from "effect/Stream";
import type * as PlatformError from "./Error.js";
import type * as FileSystem from "./FileSystem.js";
import type * as Headers from "./Headers.js";
import type * as Body from "./HttpBody.js";
import type { HttpClient } from "./HttpClient.js";
import type { HttpClientError } from "./HttpClientError.js";
import type { HttpClientResponse } from "./HttpClientResponse.js";
import type { HttpMethod } from "./HttpMethod.js";
import type * as UrlParams from "./UrlParams.js";
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
 * @category models
 */
export interface HttpClientRequest extends Effect.Effect<HttpClientResponse, HttpClientError, HttpClient.Default | Scope>, Inspectable {
    readonly [TypeId]: TypeId;
    readonly method: HttpMethod;
    readonly url: string;
    readonly urlParams: UrlParams.UrlParams;
    readonly hash: Option.Option<string>;
    readonly headers: Headers.Headers;
    readonly body: Body.HttpBody;
}
/**
 * @since 1.0.0
 * @category models
 */
export interface Options {
    readonly method?: HttpMethod | undefined;
    readonly url?: string | URL | undefined;
    readonly urlParams?: UrlParams.Input | undefined;
    readonly hash?: string | undefined;
    readonly headers?: Headers.Input | undefined;
    readonly body?: Body.HttpBody | undefined;
    readonly accept?: string | undefined;
    readonly acceptJson?: boolean | undefined;
}
/**
 * @since 1.0.0
 */
export declare namespace Options {
    /**
     * @since 1.0.0
     * @category models
     */
    interface NoBody extends Omit<Options, "method" | "url" | "body"> {
    }
    /**
     * @since 1.0.0
     * @category models
     */
    interface NoUrl extends Omit<Options, "method" | "url"> {
    }
}
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const make: <M extends HttpMethod>(method: M) => (url: string | URL, options?: (M extends "GET" | "HEAD" ? Options.NoBody : Options.NoUrl) | undefined) => HttpClientRequest;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const get: (url: string | URL, options?: Options.NoBody) => HttpClientRequest;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const post: (url: string | URL, options?: Options.NoUrl) => HttpClientRequest;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const patch: (url: string | URL, options?: Options.NoUrl) => HttpClientRequest;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const put: (url: string | URL, options?: Options.NoUrl) => HttpClientRequest;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const del: (url: string | URL, options?: Options.NoUrl) => HttpClientRequest;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const head: (url: string | URL, options?: Options.NoBody) => HttpClientRequest;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const options: (url: string | URL, options?: Options.NoUrl) => HttpClientRequest;
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const modify: {
    (options: Options): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, options: Options): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const setMethod: {
    (method: HttpMethod): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, method: HttpMethod): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const setHeader: {
    (key: string, value: string): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, key: string, value: string): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const setHeaders: {
    (input: Headers.Input): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, input: Headers.Input): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const basicAuth: {
    (username: string, password: string): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, username: string, password: string): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const bearerToken: {
    (token: string): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, token: string): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const accept: {
    (mediaType: string): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, mediaType: string): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const acceptJson: (self: HttpClientRequest) => HttpClientRequest;
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const setUrl: {
    (url: string | URL): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, url: string | URL): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const prependUrl: {
    (path: string): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, path: string): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const appendUrl: {
    (path: string): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, path: string): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const updateUrl: {
    (f: (url: string) => string): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, f: (url: string) => string): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const setUrlParam: {
    (key: string, value: string): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, key: string, value: string): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const setUrlParams: {
    (input: UrlParams.Input): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, input: UrlParams.Input): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const appendUrlParam: {
    (key: string, value: string): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, key: string, value: string): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const appendUrlParams: {
    (input: UrlParams.Input): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, input: UrlParams.Input): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const setHash: {
    (hash: string): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, hash: string): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const removeHash: (self: HttpClientRequest) => HttpClientRequest;
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const setBody: {
    (body: Body.HttpBody): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, body: Body.HttpBody): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const uint8ArrayBody: {
    (body: Uint8Array, contentType?: string): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, body: Uint8Array, contentType?: string): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const textBody: {
    (body: string, contentType?: string): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, body: string, contentType?: string): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const jsonBody: {
    (body: unknown): (self: HttpClientRequest) => Effect.Effect<HttpClientRequest, Body.HttpBodyError>;
    (self: HttpClientRequest, body: unknown): Effect.Effect<HttpClientRequest, Body.HttpBodyError>;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const unsafeJsonBody: {
    (body: unknown): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, body: unknown): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const schemaBody: <A, I, R>(schema: Schema.Schema<A, I, R>, options?: ParseOptions | undefined) => {
    (body: A): (self: HttpClientRequest) => Effect.Effect<HttpClientRequest, Body.HttpBodyError, R>;
    (self: HttpClientRequest, body: A): Effect.Effect<HttpClientRequest, Body.HttpBodyError, R>;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const urlParamsBody: {
    (input: UrlParams.Input): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, input: UrlParams.Input): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const formDataBody: {
    (body: FormData): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, body: FormData): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const streamBody: {
    (body: Stream.Stream<Uint8Array, unknown>, options?: {
        readonly contentType?: string | undefined;
        readonly contentLength?: number | undefined;
    } | undefined): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, body: Stream.Stream<Uint8Array, unknown>, options?: {
        readonly contentType?: string | undefined;
        readonly contentLength?: number | undefined;
    } | undefined): HttpClientRequest;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const fileBody: {
    (path: string, options?: FileSystem.StreamOptions & {
        readonly contentType?: string;
    }): (self: HttpClientRequest) => Effect.Effect<HttpClientRequest, PlatformError.PlatformError, FileSystem.FileSystem>;
    (self: HttpClientRequest, path: string, options?: FileSystem.StreamOptions & {
        readonly contentType?: string;
    }): Effect.Effect<HttpClientRequest, PlatformError.PlatformError, FileSystem.FileSystem>;
};
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const fileWebBody: {
    (file: Body.HttpBody.FileLike): (self: HttpClientRequest) => HttpClientRequest;
    (self: HttpClientRequest, file: Body.HttpBody.FileLike): HttpClientRequest;
};
//# sourceMappingURL=HttpClientRequest.d.ts.map