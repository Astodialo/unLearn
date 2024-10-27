"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.withPreResponseHandler = exports.toWebHandlerRuntime = exports.toWebHandlerLayer = exports.toWebHandler = exports.toHandled = exports.currentPreResponseHandlers = exports.appendPreResponseHandler = void 0;
var Context = _interopRequireWildcard(require("effect/Context"));
var Effect = _interopRequireWildcard(require("effect/Effect"));
var Exit = _interopRequireWildcard(require("effect/Exit"));
var FiberRef = _interopRequireWildcard(require("effect/FiberRef"));
var _Function = require("effect/Function");
var _GlobalValue = require("effect/GlobalValue");
var Layer = _interopRequireWildcard(require("effect/Layer"));
var Option = _interopRequireWildcard(require("effect/Option"));
var Runtime = _interopRequireWildcard(require("effect/Runtime"));
var Scope = _interopRequireWildcard(require("effect/Scope"));
var ServerError = _interopRequireWildcard(require("./HttpServerError.js"));
var ServerRequest = _interopRequireWildcard(require("./HttpServerRequest.js"));
var ServerResponse = _interopRequireWildcard(require("./HttpServerResponse.js"));
var internalMiddleware = _interopRequireWildcard(require("./internal/httpMiddleware.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 */

/**
 * @since 1.0.0
 * @category combinators
 */
const toHandled = (self, handleResponse, middleware) => {
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
exports.toHandled = toHandled;
const currentPreResponseHandlers = exports.currentPreResponseHandlers = /*#__PURE__*/(0, _GlobalValue.globalValue)( /*#__PURE__*/Symbol.for("@effect/platform/HttpApp/preResponseHandlers"), () => FiberRef.unsafeMake(Option.none()));
/**
 * @since 1.0.0
 * @category fiber refs
 */
const appendPreResponseHandler = handler => FiberRef.update(currentPreResponseHandlers, Option.match({
  onNone: () => Option.some(handler),
  onSome: prev => Option.some((request, response) => Effect.flatMap(prev(request, response), response => handler(request, response)))
}));
/**
 * @since 1.0.0
 * @category fiber refs
 */
exports.appendPreResponseHandler = appendPreResponseHandler;
const withPreResponseHandler = exports.withPreResponseHandler = /*#__PURE__*/(0, _Function.dual)(2, (self, handler) => Effect.locallyWith(self, currentPreResponseHandlers, Option.match({
  onNone: () => Option.some(handler),
  onSome: prev => Option.some((request, response) => Effect.flatMap(prev(request, response), response => handler(request, response)))
})));
/**
 * @since 1.0.0
 * @category conversions
 */
const toWebHandlerRuntime = runtime => {
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
exports.toWebHandlerRuntime = toWebHandlerRuntime;
const toWebHandler = exports.toWebHandler = /*#__PURE__*/toWebHandlerRuntime(Runtime.defaultRuntime);
/**
 * @since 1.0.0
 * @category conversions
 */
const toWebHandlerLayer = (self, layer, middleware) => {
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
exports.toWebHandlerLayer = toWebHandlerLayer;
//# sourceMappingURL=HttpApp.js.map