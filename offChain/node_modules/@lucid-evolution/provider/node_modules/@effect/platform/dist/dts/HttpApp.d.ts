import * as Effect from "effect/Effect";
import * as FiberRef from "effect/FiberRef";
import * as Layer from "effect/Layer";
import * as Option from "effect/Option";
import * as Runtime from "effect/Runtime";
import * as Scope from "effect/Scope";
import type { HttpMiddleware } from "./HttpMiddleware.js";
import * as ServerError from "./HttpServerError.js";
import * as ServerRequest from "./HttpServerRequest.js";
import * as ServerResponse from "./HttpServerResponse.js";
/**
 * @since 1.0.0
 * @category models
 */
export interface HttpApp<A = ServerResponse.HttpServerResponse, E = never, R = never> extends Effect.Effect<A, E, R | ServerRequest.HttpServerRequest> {
}
/**
 * @since 1.0.0
 * @category models
 */
export type Default<E = never, R = never> = HttpApp<ServerResponse.HttpServerResponse, E, R>;
/**
 * @since 1.0.0
 * @category combinators
 */
export declare const toHandled: <E, R, _, EH, RH>(self: Default<E, R>, handleResponse: (request: ServerRequest.HttpServerRequest, response: ServerResponse.HttpServerResponse) => Effect.Effect<_, EH, RH>, middleware?: HttpMiddleware | undefined) => Effect.Effect<void, never, Exclude<R | RH, Scope.Scope>>;
/**
 * @since 1.0.0
 * @category models
 */
export type PreResponseHandler = (request: ServerRequest.HttpServerRequest, response: ServerResponse.HttpServerResponse) => Effect.Effect<ServerResponse.HttpServerResponse, ServerError.ResponseError>;
/**
 * @since 1.0.0
 * @category fiber refs
 */
export declare const currentPreResponseHandlers: FiberRef.FiberRef<Option.Option<PreResponseHandler>>;
/**
 * @since 1.0.0
 * @category fiber refs
 */
export declare const appendPreResponseHandler: (handler: PreResponseHandler) => Effect.Effect<void>;
/**
 * @since 1.0.0
 * @category fiber refs
 */
export declare const withPreResponseHandler: ((handler: PreResponseHandler) => <A, E, R>(self: HttpApp<A, E, R>) => HttpApp<A, E, R>) & (<A, E, R>(self: HttpApp<A, E, R>, handler: PreResponseHandler) => HttpApp<A, E, R>);
/**
 * @since 1.0.0
 * @category conversions
 */
export declare const toWebHandlerRuntime: <R>(runtime: Runtime.Runtime<R>) => <E>(self: Default<E, R | Scope.Scope>, middleware?: HttpMiddleware | undefined) => (request: Request) => Promise<Response>;
/**
 * @since 1.0.0
 * @category conversions
 */
export declare const toWebHandler: <E>(self: Default<E, Scope.Scope>, middleware?: HttpMiddleware | undefined) => (request: Request) => Promise<Response>;
/**
 * @since 1.0.0
 * @category conversions
 */
export declare const toWebHandlerLayer: <E, R, RE>(self: Default<E, R | Scope.Scope>, layer: Layer.Layer<R, RE>, middleware?: HttpMiddleware | undefined) => {
    readonly close: () => Promise<void>;
    readonly handler: (request: Request) => Promise<Response>;
};
//# sourceMappingURL=HttpApp.d.ts.map