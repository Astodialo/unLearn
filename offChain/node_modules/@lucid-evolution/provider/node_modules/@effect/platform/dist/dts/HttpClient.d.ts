/**
 * @since 1.0.0
 */
import type { ParseOptions } from "@effect/schema/AST";
import type * as ParseResult from "@effect/schema/ParseResult";
import type * as Schema from "@effect/schema/Schema";
import type * as Context from "effect/Context";
import type * as Effect from "effect/Effect";
import type { RuntimeFiber } from "effect/Fiber";
import type * as FiberRef from "effect/FiberRef";
import type { Inspectable } from "effect/Inspectable";
import type * as Layer from "effect/Layer";
import type { Pipeable } from "effect/Pipeable";
import type * as Predicate from "effect/Predicate";
import type { Ref } from "effect/Ref";
import type * as Schedule from "effect/Schedule";
import type * as Scope from "effect/Scope";
import type { Cookies } from "./Cookies.js";
import type * as Error from "./HttpClientError.js";
import type * as ClientRequest from "./HttpClientRequest.js";
import type * as ClientResponse from "./HttpClientResponse.js";
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
export interface HttpClient<A = ClientResponse.HttpClientResponse, E = never, R = never> extends Pipeable, Inspectable {
    (request: ClientRequest.HttpClientRequest): Effect.Effect<A, E, R>;
    readonly [TypeId]: TypeId;
    readonly preprocess: HttpClient.Preprocess<E, R>;
    readonly execute: HttpClient.Execute<A, E, R>;
}
/**
 * @since 1.0.0
 */
export declare namespace HttpClient {
    /**
     * @since 1.0.0
     * @category models
     */
    type Preprocess<E, R> = (request: ClientRequest.HttpClientRequest) => Effect.Effect<ClientRequest.HttpClientRequest, E, R>;
    /**
     * @since 1.0.0
     * @category models
     */
    type Execute<A, E = never, R = never> = (request: Effect.Effect<ClientRequest.HttpClientRequest, E, R>) => Effect.Effect<A, E, R>;
    /**
     * @since 1.0.0
     * @category models
     */
    type WithResponse<E = never, R = never> = HttpClient<ClientResponse.HttpClientResponse, E, R>;
    /**
     * @since 1.0.0
     * @category models
     */
    type Default = WithResponse<Error.HttpClientError, Scope.Scope>;
}
/**
 * @since 1.0.0
 * @category models
 */
export interface Fetch {
    readonly _: unique symbol;
}
/**
 * @since 1.0.0
 * @category tags
 */
export declare const HttpClient: Context.Tag<HttpClient.Default, HttpClient.Default>;
/**
 * @since 1.0.0
 * @category tags
 */
export declare const Fetch: Context.Tag<Fetch, typeof globalThis.fetch>;
/**
 * @since 1.0.0
 * @category layers
 */
export declare const layer: Layer.Layer<HttpClient.Default>;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const fetch: HttpClient.Default;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const fetchOk: HttpClient.Default;
/**
 * @since 1.0.0
 * @category error handling
 */
export declare const catchAll: {
    <E, E2, R2, A2>(f: (e: E) => Effect.Effect<A2, E2, R2>): <A, R>(self: HttpClient<A, E, R>) => HttpClient<A2 | A, E2, R2 | R>;
    <A, E, R, A2, E2, R2>(self: HttpClient<A, E, R>, f: (e: E) => Effect.Effect<A2, E2, R2>): HttpClient<A | A2, E2, R | R2>;
};
/**
 * @since 1.0.0
 * @category error handling
 */
export declare const catchTag: {
    <E extends {
        _tag: string;
    }, K extends E["_tag"] & string, A1, E1, R1>(tag: K, f: (e: Extract<E, {
        _tag: K;
    }>) => Effect.Effect<A1, E1, R1>): <A, R>(self: HttpClient<A, E, R>) => HttpClient<A1 | A, E1 | Exclude<E, {
        _tag: K;
    }>, R1 | R>;
    <A, E extends {
        _tag: string;
    }, R, K extends E["_tag"] & string, A1, E1, R1>(self: HttpClient<A, E, R>, tag: K, f: (e: Extract<E, {
        _tag: K;
    }>) => Effect.Effect<A1, E1, R1>): HttpClient<A | A1, E1 | Exclude<E, {
        _tag: K;
    }>, R | R1>;
};
/**
 * @since 1.0.0
 * @category error handling
 */
export declare const catchTags: {
    <E extends {
        _tag: string;
    }, Cases extends {
        [K in E["_tag"]]+?: ((error: Extract<E, {
            _tag: K;
        }>) => Effect.Effect<any, any, any>) | undefined;
    }>(cases: Cases): <A, R>(self: HttpClient<A, E, R>) => HttpClient<A | {
        [K in keyof Cases]: Cases[K] extends (...args: Array<any>) => Effect.Effect<infer A, any, any> ? A : never;
    }[keyof Cases], Exclude<E, {
        _tag: keyof Cases;
    }> | {
        [K in keyof Cases]: Cases[K] extends (...args: Array<any>) => Effect.Effect<any, infer E, any> ? E : never;
    }[keyof Cases], R | {
        [K in keyof Cases]: Cases[K] extends (...args: Array<any>) => Effect.Effect<any, any, infer R> ? R : never;
    }[keyof Cases]>;
    <A, E extends {
        _tag: string;
    }, R, Cases extends {
        [K in E["_tag"]]+?: ((error: Extract<E, {
            _tag: K;
        }>) => Effect.Effect<any, any, any>) | undefined;
    }>(self: HttpClient<A, E, R>, cases: Cases): HttpClient<A | {
        [K in keyof Cases]: Cases[K] extends (...args: Array<any>) => Effect.Effect<infer A, any, any> ? A : never;
    }[keyof Cases], Exclude<E, {
        _tag: keyof Cases;
    }> | {
        [K in keyof Cases]: Cases[K] extends (...args: Array<any>) => Effect.Effect<any, infer E, any> ? E : never;
    }[keyof Cases], R | {
        [K in keyof Cases]: Cases[K] extends (...args: Array<any>) => Effect.Effect<any, any, infer R> ? R : never;
    }[keyof Cases]>;
};
/**
 * @since 1.0.0
 * @category filters
 */
export declare const filterOrElse: {
    <A, B, E2, R2>(f: Predicate.Predicate<A>, orElse: (a: A) => Effect.Effect<B, E2, R2>): <E, R>(self: HttpClient<A, E, R>) => HttpClient<A | B, E2 | E, R2 | R>;
    <A, E, R, B, E2, R2>(self: HttpClient<A, E, R>, f: Predicate.Predicate<A>, orElse: (a: A) => Effect.Effect<B, E2, R2>): HttpClient<A | B, E | E2, R | R2>;
};
/**
 * @since 1.0.0
 * @category filters
 */
export declare const filterOrFail: {
    <A, E2>(f: Predicate.Predicate<A>, orFailWith: (a: A) => E2): <E, R>(self: HttpClient<A, E, R>) => HttpClient<A, E2 | E, R>;
    <A, E, R, E2>(self: HttpClient<A, E, R>, f: Predicate.Predicate<A>, orFailWith: (a: A) => E2): HttpClient<A, E | E2, R>;
};
/**
 * @since 1.0.0
 * @category filters
 */
export declare const filterStatus: {
    (f: (status: number) => boolean): <E, R>(self: HttpClient.WithResponse<E, R>) => HttpClient.WithResponse<E | Error.ResponseError, R>;
    <E, R>(self: HttpClient.WithResponse<E, R>, f: (status: number) => boolean): HttpClient.WithResponse<Error.ResponseError | E, R>;
};
/**
 * @since 1.0.0
 * @category filters
 */
export declare const filterStatusOk: <E, R>(self: HttpClient.WithResponse<E, R>) => HttpClient.WithResponse<Error.ResponseError | E, R>;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const make: <A, E, R, E2, R2>(execute: (request: Effect.Effect<ClientRequest.HttpClientRequest, E2, R2>) => Effect.Effect<A, E, R>, preprocess: HttpClient.Preprocess<E2, R2>) => HttpClient<A, E, R>;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const makeDefault: (f: (request: ClientRequest.HttpClientRequest, url: URL, signal: AbortSignal, fiber: RuntimeFiber<ClientResponse.HttpClientResponse, Error.HttpClientError>) => Effect.Effect<ClientResponse.HttpClientResponse, Error.HttpClientError, Scope.Scope>) => HttpClient.Default;
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
export declare const transform: {
    <A, E, R, A1, E1, R1>(f: (effect: Effect.Effect<A, E, R>, request: ClientRequest.HttpClientRequest) => Effect.Effect<A1, E1, R1>): (self: HttpClient<A, E, R>) => HttpClient<A1, E | E1, R | R1>;
    <A, E, R, A1, E1, R1>(self: HttpClient<A, E, R>, f: (effect: Effect.Effect<A, E, R>, request: ClientRequest.HttpClientRequest) => Effect.Effect<A1, E1, R1>): HttpClient<A1, E | E1, R | R1>;
};
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
export declare const transformResponse: {
    <A, E, R, A1, E1, R1>(f: (effect: Effect.Effect<A, E, R>) => Effect.Effect<A1, E1, R1>): (self: HttpClient<A, E, R>) => HttpClient<A1, E1, R1>;
    <A, E, R, A1, E1, R1>(self: HttpClient<A, E, R>, f: (effect: Effect.Effect<A, E, R>) => Effect.Effect<A1, E1, R1>): HttpClient<A1, E1, R1>;
};
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
export declare const map: {
    <A, B>(f: (a: A) => B): <E, R>(self: HttpClient<A, E, R>) => HttpClient<B, E, R>;
    <A, E, R, B>(self: HttpClient<A, E, R>, f: (a: A) => B): HttpClient<B, E, R>;
};
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
export declare const mapEffect: {
    <A, B, E2, R2>(f: (a: A) => Effect.Effect<B, E2, R2>): <E, R>(self: HttpClient<A, E, R>) => HttpClient<B, E2 | E, R2 | R>;
    <A, E, R, B, E2, R2>(self: HttpClient<A, E, R>, f: (a: A) => Effect.Effect<B, E2, R2>): HttpClient<B, E | E2, R2 | R>;
};
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
export declare const mapEffectScoped: {
    <A, B, E2, R2>(f: (a: A) => Effect.Effect<B, E2, R2>): <E, R>(self: HttpClient<A, E, R>) => HttpClient<B, E2 | E, Exclude<R2, Scope.Scope> | Exclude<R, Scope.Scope>>;
    <A, E, R, B, E2, R2>(self: HttpClient<A, E, R>, f: (a: A) => Effect.Effect<B, E2, R2>): HttpClient<B, E | E2, Exclude<R2, Scope.Scope> | Exclude<R, Scope.Scope>>;
};
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
export declare const mapRequest: {
    (f: (a: ClientRequest.HttpClientRequest) => ClientRequest.HttpClientRequest): <A, E, R>(self: HttpClient<A, E, R>) => HttpClient<A, E, R>;
    <A, E, R>(self: HttpClient<A, E, R>, f: (a: ClientRequest.HttpClientRequest) => ClientRequest.HttpClientRequest): HttpClient<A, E, R>;
};
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
export declare const mapRequestEffect: {
    <E2, R2>(f: (a: ClientRequest.HttpClientRequest) => Effect.Effect<ClientRequest.HttpClientRequest, E2, R2>): <A, E, R>(self: HttpClient<A, E, R>) => HttpClient<A, E2 | E, R2 | R>;
    <A, E, R, E2, R2>(self: HttpClient<A, E, R>, f: (a: ClientRequest.HttpClientRequest) => Effect.Effect<ClientRequest.HttpClientRequest, E2, R2>): HttpClient<A, E | E2, R | R2>;
};
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
export declare const mapInputRequest: {
    (f: (a: ClientRequest.HttpClientRequest) => ClientRequest.HttpClientRequest): <A, E, R>(self: HttpClient<A, E, R>) => HttpClient<A, E, R>;
    <A, E, R>(self: HttpClient<A, E, R>, f: (a: ClientRequest.HttpClientRequest) => ClientRequest.HttpClientRequest): HttpClient<A, E, R>;
};
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
export declare const mapInputRequestEffect: {
    <E2, R2>(f: (a: ClientRequest.HttpClientRequest) => Effect.Effect<ClientRequest.HttpClientRequest, E2, R2>): <A, E, R>(self: HttpClient<A, E, R>) => HttpClient<A, E2 | E, R2 | R>;
    <A, E, R, E2, R2>(self: HttpClient<A, E, R>, f: (a: ClientRequest.HttpClientRequest) => Effect.Effect<ClientRequest.HttpClientRequest, E2, R2>): HttpClient<A, E | E2, R2 | R>;
};
/**
 * @since 1.0.0
 * @category error handling
 */
export declare const retry: {
    <B, E extends E0, E0, R1>(policy: Schedule.Schedule<B, E0, R1>): <A, R>(self: HttpClient<A, E, R>) => HttpClient<A, E, R1 | R>;
    <A, E extends E0, E0, R, R1, B>(self: HttpClient<A, E, R>, policy: Schedule.Schedule<B, E0, R1>): HttpClient<A, E, R1 | R>;
};
/**
 * @since 1.0.0
 * @category resources & finalizers
 */
export declare const scoped: <A, E, R>(self: HttpClient<A, E, R>) => HttpClient<A, E, Exclude<R, Scope.Scope>>;
/**
 * @since 1.0.0
 * @category schema
 */
export declare const schemaFunction: {
    <SA, SI, SR>(schema: Schema.Schema<SA, SI, SR>, options?: ParseOptions | undefined): <A, E, R>(self: HttpClient<A, E, R>) => (request: ClientRequest.HttpClientRequest) => (a: SA) => Effect.Effect<A, Error.RequestError | E | ParseResult.ParseError, SR | R>;
    <A, E, R, SA, SI, SR>(self: HttpClient<A, E, R>, schema: Schema.Schema<SA, SI, SR>, options?: ParseOptions | undefined): (request: ClientRequest.HttpClientRequest) => (a: SA) => Effect.Effect<A, Error.RequestError | ParseResult.ParseError | E, R | SR>;
};
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
export declare const tap: {
    <A, _, E2, R2>(f: (a: A) => Effect.Effect<_, E2, R2>): <E, R>(self: HttpClient<A, E, R>) => HttpClient<A, E2 | E, R2 | R>;
    <A, E, R, _, E2, R2>(self: HttpClient<A, E, R>, f: (a: A) => Effect.Effect<_, E2, R2>): HttpClient<A, E | E2, R2 | R>;
};
/**
 * @since 1.0.0
 * @category mapping & sequencing
 */
export declare const tapRequest: {
    <_, E2, R2>(f: (a: ClientRequest.HttpClientRequest) => Effect.Effect<_, E2, R2>): <A, E, R>(self: HttpClient<A, E, R>) => HttpClient<A, E2 | E, R2 | R>;
    <A, E, R, _, E2, R2>(self: HttpClient<A, E, R>, f: (a: ClientRequest.HttpClientRequest) => Effect.Effect<_, E2, R2>): HttpClient<A, E | E2, R | R2>;
};
/**
 * @since 1.0.0
 * @category cookies
 */
export declare const withCookiesRef: {
    (ref: Ref<Cookies>): <E, R>(self: HttpClient.WithResponse<E, R>) => HttpClient.WithResponse<E, R>;
    <E, R>(self: HttpClient.WithResponse<E, R>, ref: Ref<Cookies>): HttpClient.WithResponse<E, R>;
};
/**
 * @since 1.0.0
 * @category redirects
 */
export declare const followRedirects: {
    (maxRedirects?: number | undefined): <E, R>(self: HttpClient.WithResponse<E, R>) => HttpClient.WithResponse<E, R>;
    <E, R>(self: HttpClient.WithResponse<E, R>, maxRedirects?: number | undefined): HttpClient.WithResponse<E, R>;
};
/**
 * @since 1.0.0
 * @category fiber refs
 */
export declare const currentTracerDisabledWhen: FiberRef.FiberRef<Predicate.Predicate<ClientRequest.HttpClientRequest>>;
/**
 * @since 1.0.0
 * @category fiber refs
 */
export declare const withTracerDisabledWhen: {
    (predicate: Predicate.Predicate<ClientRequest.HttpClientRequest>): <A, E, R>(effect: Effect.Effect<A, E, R>) => Effect.Effect<A, E, R>;
    <A, E, R>(effect: Effect.Effect<A, E, R>, predicate: Predicate.Predicate<ClientRequest.HttpClientRequest>): Effect.Effect<A, E, R>;
};
/**
 * @since 1.0.0
 * @category fiber refs
 */
export declare const currentTracerPropagation: FiberRef.FiberRef<boolean>;
/**
 * @since 1.0.0
 * @category fiber refs
 */
export declare const withTracerPropagation: {
    (enabled: boolean): <A, E, R>(effect: Effect.Effect<A, E, R>) => Effect.Effect<A, E, R>;
    <A, E, R>(effect: Effect.Effect<A, E, R>, enabled: boolean): Effect.Effect<A, E, R>;
};
/**
 * @since 1.0.0
 * @category fiber refs
 */
export declare const currentFetchOptions: FiberRef.FiberRef<RequestInit>;
/**
 * @since 1.0.0
 * @category fiber refs
 */
export declare const withFetchOptions: {
    (options: RequestInit): <A, E, R>(effect: Effect.Effect<A, E, R>) => Effect.Effect<A, E, R>;
    <A, E, R>(effect: Effect.Effect<A, E, R>, options: RequestInit): Effect.Effect<A, E, R>;
};
//# sourceMappingURL=HttpClient.d.ts.map