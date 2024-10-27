/**
 * @since 1.0.0
 */
import * as Context from "effect/Context";
import * as Effect from "effect/Effect";
import * as Exit from "effect/Exit";
import * as FiberRef from "effect/FiberRef";
import { dual } from "effect/Function";
import { globalValue } from "effect/GlobalValue";
import * as Layer from "effect/Layer";
import * as Option from "effect/Option";
import * as Runtime from "effect/Runtime";
import * as Scope from "effect/Scope";
import * as ServerError from "./HttpServerError.js";
import * as ServerRequest from "./HttpServerRequest.js";
import * as ServerResponse from "./HttpServerResponse.js";
import * as internalMiddleware from "./internal/httpMiddleware.js";
/**
 * @since 1.0.0
 * @category combinators
 */
export const toHandled = (self, handleResponse, middleware) => {
  const responded = Effect.withFiberRuntime(fiber => {
    const request = Context.unsafeGet(fiber.getFiberRef(FiberRef.currentContext), ServerRequest.HttpServerRequest);
    const preprocessResponse = response => {
      const handler = fiber.getFiberRef(currentPreResponseHandlers);
      return handler._tag === "Some" ? handler.value(request, response) : Effect.succeed(response);
    };
    return Effect.matchCauseEffect(self, {
      onFailure: cause => Effect.flatMap(ServerError.causeResponse(cause), ([response, cause]) => preprocessResponse(response).pipe(Effect.flatMap(response => handleResponse(request, response)), Effect.zipRight(Effect.failCause(cause)))),
      onSuccess: response => Effect.tap(preprocessResponse(response), response => handleResponse(request, response))
    });
  });
  const withTracer = internalMiddleware.tracer(responded);
  return Effect.uninterruptible(Effect.catchAllCause(Effect.scoped(middleware === undefined ? withTracer : middleware(withTracer)), _ => Effect.void));
};
/**
 * @since 1.0.0
 * @category fiber refs
 */
export const currentPreResponseHandlers = /*#__PURE__*/globalValue( /*#__PURE__*/Symbol.for("@effect/platform/HttpApp/preResponseHandlers"), () => FiberRef.unsafeMake(Option.none()));
/**
 * @since 1.0.0
 * @category fiber refs
 */
export const appendPreResponseHandler = handler => FiberRef.update(currentPreResponseHandlers, Option.match({
  onNone: () => Option.some(handler),
  onSome: prev => Option.some((request, response) => Effect.flatMap(prev(request, response), response => handler(request, response)))
}));
/**
 * @since 1.0.0
 * @category fiber refs
 */
export const withPreResponseHandler = /*#__PURE__*/dual(2, (self, handler) => Effect.locallyWith(self, currentPreResponseHandlers, Option.match({
  onNone: () => Option.some(handler),
  onSome: prev => Option.some((request, response) => Effect.flatMap(prev(request, response), response => handler(request, response)))
})));
/**
 * @since 1.0.0
 * @category conversions
 */
export const toWebHandlerRuntime = runtime => {
  const run = Runtime.runFork(runtime);
  return (self, middleware) => request => new Promise(resolve => {
    const fiber = run(Effect.provideService(toHandled(self, (request, response) => {
      resolve(ServerResponse.toWeb(response, {
        withoutBody: request.method === "HEAD",
        runtime
      }));
      return Effect.void;
    }, middleware), ServerRequest.HttpServerRequest, ServerRequest.fromWeb(request)));
    request.signal.addEventListener("abort", () => {
      fiber.unsafeInterruptAsFork(ServerError.clientAbortFiberId);
    }, {
      once: true
    });
  });
};
/**
 * @since 1.0.0
 * @category conversions
 */
export const toWebHandler = /*#__PURE__*/toWebHandlerRuntime(Runtime.defaultRuntime);
/**
 * @since 1.0.0
 * @category conversions
 */
export const toWebHandlerLayer = (self, layer, middleware) => {
  const scope = Effect.runSync(Scope.make());
  const close = () => Effect.runPromise(Scope.close(scope, Exit.void));
  const build = Effect.map(Layer.toRuntime(layer), _ => toWebHandlerRuntime(_)(self, middleware));
  const runner = Effect.runPromise(Scope.extend(build, scope));
  const handler = request => runner.then(handler => handler(request));
  return {
    close,
    handler
  };
};
//# sourceMappingURL=HttpApp.js.map