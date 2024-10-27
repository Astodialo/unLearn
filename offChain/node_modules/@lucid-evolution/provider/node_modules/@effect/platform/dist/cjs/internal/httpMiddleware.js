"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.xForwardedHeaders = exports.withTracerDisabledWhenEffect = exports.withTracerDisabledWhen = exports.withTracerDisabledForUrls = exports.withLoggerDisabled = exports.tracer = exports.searchParamsParser = exports.make = exports.loggerDisabled = exports.logger = exports.currentTracerDisabledWhen = exports.cors = void 0;
var Context = _interopRequireWildcard(require("effect/Context"));
var Effect = _interopRequireWildcard(require("effect/Effect"));
var FiberRef = _interopRequireWildcard(require("effect/FiberRef"));
var _Function = require("effect/Function");
var _GlobalValue = require("effect/GlobalValue");
var Layer = _interopRequireWildcard(require("effect/Layer"));
var Option = _interopRequireWildcard(require("effect/Option"));
var Headers = _interopRequireWildcard(require("../Headers.js"));
var ServerError = _interopRequireWildcard(require("../HttpServerError.js"));
var ServerRequest = _interopRequireWildcard(require("../HttpServerRequest.js"));
var ServerResponse = _interopRequireWildcard(require("../HttpServerResponse.js"));
var TraceContext = _interopRequireWildcard(require("../HttpTraceContext.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const make = middleware => middleware;
/** @internal */
exports.make = make;
const loggerDisabled = exports.loggerDisabled = /*#__PURE__*/(0, _GlobalValue.globalValue)( /*#__PURE__*/Symbol.for("@effect/platform/HttpMiddleware/loggerDisabled"), () => FiberRef.unsafeMake(false));
/** @internal */
const withLoggerDisabled = self => Effect.zipRight(FiberRef.set(loggerDisabled, true), self);
/** @internal */
exports.withLoggerDisabled = withLoggerDisabled;
const currentTracerDisabledWhen = exports.currentTracerDisabledWhen = /*#__PURE__*/(0, _GlobalValue.globalValue)( /*#__PURE__*/Symbol.for("@effect/platform/HttpMiddleware/tracerDisabledWhen"), () => FiberRef.unsafeMake(_Function.constFalse));
/** @internal */
const withTracerDisabledWhen = exports.withTracerDisabledWhen = /*#__PURE__*/(0, _Function.dual)(2, (self, pred) => Layer.locally(self, currentTracerDisabledWhen, pred));
/** @internal */
const withTracerDisabledWhenEffect = exports.withTracerDisabledWhenEffect = /*#__PURE__*/(0, _Function.dual)(2, (self, pred) => Effect.locally(self, currentTracerDisabledWhen, pred));
/** @internal */
const withTracerDisabledForUrls = exports.withTracerDisabledForUrls = /*#__PURE__*/(0, _Function.dual)(2, (self, urls) => Layer.locally(self, currentTracerDisabledWhen, req => urls.includes(req.url)));
/** @internal */
const logger = exports.logger = /*#__PURE__*/make(httpApp => {
  let counter = 0;
  return Effect.withFiberRuntime(fiber => {
    const context = fiber.getFiberRef(FiberRef.currentContext);
    const request = Context.unsafeGet(context, ServerRequest.HttpServerRequest);
    return Effect.withLogSpan(Effect.flatMap(Effect.exit(httpApp), exit => {
      if (fiber.getFiberRef(loggerDisabled)) {
        return exit;
      } else if (exit._tag === "Failure") {
        const [response, cause] = ServerError.causeResponseStripped(exit.cause);
        return Effect.zipRight(Effect.annotateLogs(Effect.log(cause._tag === "Some" ? cause.value : "Sent HTTP Response"), {
          "http.method": request.method,
          "http.url": request.url,
          "http.status": response.status
        }), exit);
      }
      return Effect.zipRight(Effect.annotateLogs(Effect.log("Sent HTTP response"), {
        "http.method": request.method,
        "http.url": request.url,
        "http.status": exit.value.status
      }), exit);
    }), `http.span.${++counter}`);
  });
});
/** @internal */
const tracer = exports.tracer = /*#__PURE__*/make(httpApp => Effect.withFiberRuntime(fiber => {
  const context = fiber.getFiberRef(FiberRef.currentContext);
  const request = Context.unsafeGet(context, ServerRequest.HttpServerRequest);
  const disabled = fiber.getFiberRef(currentTracerDisabledWhen)(request);
  if (disabled) {
    return httpApp;
  }
  const url = Option.getOrUndefined(ServerRequest.toURL(request));
  if (url !== undefined && (url.username !== "" || url.password !== "")) {
    url.username = "REDACTED";
    url.password = "REDACTED";
  }
  const redactedHeaderNames = fiber.getFiberRef(Headers.currentRedactedNames);
  const redactedHeaders = Headers.redact(request.headers, redactedHeaderNames);
  return Effect.useSpan(`http.server ${request.method}`, {
    parent: Option.getOrUndefined(TraceContext.fromHeaders(request.headers)),
    kind: "server",
    captureStackTrace: false
  }, span => {
    span.attribute("http.request.method", request.method);
    if (url !== undefined) {
      span.attribute("url.full", url.toString());
      span.attribute("url.path", url.pathname);
      const query = url.search.slice(1);
      if (query !== "") {
        span.attribute("url.query", url.search.slice(1));
      }
      span.attribute("url.scheme", url.protocol.slice(0, -1));
    }
    if (request.headers["user-agent"] !== undefined) {
      span.attribute("user_agent.original", request.headers["user-agent"]);
    }
    for (const name in redactedHeaders) {
      span.attribute(`http.request.header.${name}`, String(redactedHeaders[name]));
    }
    if (request.remoteAddress._tag === "Some") {
      span.attribute("client.address", request.remoteAddress.value);
    }
    return Effect.flatMap(Effect.exit(Effect.withParentSpan(httpApp, span)), exit => {
      const response = ServerError.exitResponse(exit);
      span.attribute("http.response.status_code", response.status);
      const redactedHeaders = Headers.redact(response.headers, redactedHeaderNames);
      for (const name in redactedHeaders) {
        span.attribute(`http.response.header.${name}`, String(redactedHeaders[name]));
      }
      return exit;
    });
  });
}));
/** @internal */
const xForwardedHeaders = exports.xForwardedHeaders = /*#__PURE__*/make(httpApp => Effect.updateService(httpApp, ServerRequest.HttpServerRequest, request => request.headers["x-forwarded-host"] ? request.modify({
  headers: Headers.set(request.headers, "host", request.headers["x-forwarded-host"]),
  remoteAddress: request.headers["x-forwarded-for"]?.split(",")[0].trim()
}) : request));
/** @internal */
const searchParamsParser = httpApp => Effect.withFiberRuntime(fiber => {
  const context = fiber.getFiberRef(FiberRef.currentContext);
  const request = Context.unsafeGet(context, ServerRequest.HttpServerRequest);
  const params = ServerRequest.searchParamsFromURL(new URL(request.url));
  return Effect.locally(httpApp, FiberRef.currentContext, Context.add(context, ServerRequest.ParsedSearchParams, params));
});
/** @internal */
exports.searchParamsParser = searchParamsParser;
const cors = options => {
  const opts = {
    allowedOrigins: ["*"],
    allowedMethods: ["GET", "HEAD", "PUT", "PATCH", "POST", "DELETE"],
    allowedHeaders: [],
    exposedHeaders: [],
    credentials: false,
    ...options
  };
  const isAllowedOrigin = origin => opts.allowedOrigins.includes(origin);
  const allowOrigin = originHeader => {
    if (opts.allowedOrigins.length === 0) {
      return {
        "access-control-allow-origin": "*"
      };
    }
    if (opts.allowedOrigins.length === 1) {
      return {
        "access-control-allow-origin": opts.allowedOrigins[0],
        vary: "Origin"
      };
    }
    if (isAllowedOrigin(originHeader)) {
      return {
        "access-control-allow-origin": originHeader,
        vary: "Origin"
      };
    }
    return undefined;
  };
  const allowMethods = opts.allowedMethods.length > 0 ? {
    "access-control-allow-methods": opts.allowedMethods.join(", ")
  } : undefined;
  const allowCredentials = opts.credentials ? {
    "access-control-allow-credentials": "true"
  } : undefined;
  const allowHeaders = accessControlRequestHeaders => {
    if (opts.allowedHeaders.length === 0 && accessControlRequestHeaders) {
      return {
        vary: "Access-Control-Request-Headers",
        "access-control-allow-headers": accessControlRequestHeaders
      };
    }
    if (opts.allowedHeaders) {
      return {
        "access-control-allow-headers": opts.allowedHeaders.join(",")
      };
    }
    return undefined;
  };
  const exposeHeaders = opts.exposedHeaders.length > 0 ? {
    "access-control-expose-headers": opts.exposedHeaders.join(",")
  } : undefined;
  const maxAge = opts.maxAge ? {
    "access-control-max-age": opts.maxAge.toString()
  } : undefined;
  return httpApp => Effect.withFiberRuntime(fiber => {
    const context = fiber.getFiberRef(FiberRef.currentContext);
    const request = Context.unsafeGet(context, ServerRequest.HttpServerRequest);
    const origin = request.headers["origin"];
    const accessControlRequestHeaders = request.headers["access-control-request-headers"];
    const corsHeaders = Headers.unsafeFromRecord({
      ...allowOrigin(origin),
      ...allowCredentials,
      ...exposeHeaders
    });
    if (request.method === "OPTIONS") {
      Object.assign(corsHeaders, {
        ...allowMethods,
        ...allowHeaders(accessControlRequestHeaders),
        ...maxAge
      });
      return Effect.succeed(ServerResponse.empty({
        status: 204,
        headers: corsHeaders
      }));
    }
    return Effect.map(httpApp, ServerResponse.setHeaders(corsHeaders));
  });
};
exports.cors = cors;
//# sourceMappingURL=httpMiddleware.js.map