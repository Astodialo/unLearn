/**
 * @since 1.0.0
 */
import type * as Effect from "effect/Effect";
import type * as FiberRef from "effect/FiberRef";
import type * as Layer from "effect/Layer";
import type * as Predicate from "effect/Predicate";
import type * as App from "./HttpApp.js";
import type * as ServerRequest from "./HttpServerRequest.js";
/**
 * @since 1.0.0
 * @category models
 */
export interface HttpMiddleware {
    <E, R>(self: App.Default<E, R>): App.Default<any, any>;
}
/**
 * @since 1.0.0
 */
export declare namespace HttpMiddleware {
    /**
     * @since 1.0.0
     */
    interface Applied<A extends App.Default<any, any>, E, R> {
        (self: App.Default<E, R>): A;
    }
}
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const make: <M extends HttpMiddleware>(middleware: M) => M;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const logger: <E, R>(httpApp: App.Default<E, R>) => App.Default<E, R>;
/**
 * @since 1.0.0
 * @category fiber refs
 */
export declare const loggerDisabled: FiberRef.FiberRef<boolean>;
/**
 * @since 1.0.0
 * @category fiber refs
 */
export declare const withLoggerDisabled: <A, E, R>(self: Effect.Effect<A, E, R>) => Effect.Effect<A, E, R>;
/**
 * @since 1.0.0
 * @category fiber refs
 */
export declare const currentTracerDisabledWhen: FiberRef.FiberRef<Predicate.Predicate<ServerRequest.HttpServerRequest>>;
/**
 * @since 1.0.0
 * @category fiber refs
 */
export declare const withTracerDisabledWhen: {
    (predicate: Predicate.Predicate<ServerRequest.HttpServerRequest>): <A, E, R>(layer: Layer.Layer<A, E, R>) => Layer.Layer<A, E, R>;
    <A, E, R>(layer: Layer.Layer<A, E, R>, predicate: Predicate.Predicate<ServerRequest.HttpServerRequest>): Layer.Layer<A, E, R>;
};
/**
 * @since 1.0.0
 * @category fiber refs
 */
export declare const withTracerDisabledWhenEffect: {
    (predicate: Predicate.Predicate<ServerRequest.HttpServerRequest>): <A, E, R>(effect: Effect.Effect<A, E, R>) => Effect.Effect<A, E, R>;
    <A, E, R>(effect: Effect.Effect<A, E, R>, predicate: Predicate.Predicate<ServerRequest.HttpServerRequest>): Effect.Effect<A, E, R>;
};
/**
 * @since 1.0.0
 * @category fiber refs
 */
export declare const withTracerDisabledForUrls: {
    (urls: ReadonlyArray<string>): <A, E, R>(layer: Layer.Layer<A, E, R>) => Layer.Layer<A, E, R>;
    <A, E, R>(layer: Layer.Layer<A, E, R>, urls: ReadonlyArray<string>): Layer.Layer<A, E, R>;
};
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const xForwardedHeaders: <E, R>(httpApp: App.Default<E, R>) => App.Default<E, R>;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const searchParamsParser: <E, R>(httpApp: App.Default<E, R>) => App.Default<E, Exclude<R, ServerRequest.ParsedSearchParams>>;
/**
 * @since 1.0.0
 * @category constructors
 */
export declare const cors: (options?: {
    readonly allowedOrigins?: ReadonlyArray<string> | undefined;
    readonly allowedMethods?: ReadonlyArray<string> | undefined;
    readonly allowedHeaders?: ReadonlyArray<string> | undefined;
    readonly exposedHeaders?: ReadonlyArray<string> | undefined;
    readonly maxAge?: number | undefined;
    readonly credentials?: boolean | undefined;
} | undefined) => <E, R>(httpApp: App.Default<E, R>) => App.Default<E, R>;
//# sourceMappingURL=HttpMiddleware.d.ts.map