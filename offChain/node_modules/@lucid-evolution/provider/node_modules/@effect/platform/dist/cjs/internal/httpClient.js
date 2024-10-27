"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.withTracerPropagation = exports.withTracerDisabledWhen = exports.withFetchOptions = exports.withCookiesRef = exports.transformResponse = exports.transform = exports.tapRequest = exports.tap = exports.tag = exports.scoped = exports.schemaFunction = exports.retry = exports.mapRequestEffect = exports.mapRequest = exports.mapInputRequestEffect = exports.mapInputRequest = exports.mapEffectScoped = exports.mapEffect = exports.map = exports.makeDefault = exports.make = exports.layer = exports.followRedirects = exports.filterStatusOk = exports.filterStatus = exports.filterOrFail = exports.filterOrElse = exports.fetchOk = exports.fetch = exports.currentTracerPropagation = exports.currentTracerDisabledWhen = exports.currentFetchOptions = exports.catchTags = exports.catchTag = exports.catchAll = exports.TypeId = exports.Fetch = void 0;
var Schema = _interopRequireWildcard(require("@effect/schema/Schema"));
var Context = _interopRequireWildcard(require("effect/Context"));
var Effect = _interopRequireWildcard(require("effect/Effect"));
var FiberRef = _interopRequireWildcard(require("effect/FiberRef"));
var _Function = require("effect/Function");
var _GlobalValue = require("effect/GlobalValue");
var Layer = _interopRequireWildcard(require("effect/Layer"));
var _Pipeable = require("effect/Pipeable");
var Predicate = _interopRequireWildcard(require("effect/Predicate"));
var Ref = _interopRequireWildcard(require("effect/Ref"));
var Scope = _interopRequireWildcard(require("effect/Scope"));
var Stream = _interopRequireWildcard(require("effect/Stream"));
var Cookies = _interopRequireWildcard(require("../Cookies.js"));
var Headers = _interopRequireWildcard(require("../Headers.js"));
var Error = _interopRequireWildcard(require("../HttpClientError.js"));
var Method = _interopRequireWildcard(require("../HttpMethod.js"));
var TraceContext = _interopRequireWildcard(require("../HttpTraceContext.js"));
var UrlParams = _interopRequireWildcard(require("../UrlParams.js"));
var internalBody = _interopRequireWildcard(require("./httpBody.js"));
var internalRequest = _interopRequireWildcard(require("./httpClientRequest.js"));
var internalResponse = _interopRequireWildcard(require("./httpClientResponse.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("@effect/platform/HttpClient");
/** @internal */
const tag = exports.tag = /*#__PURE__*/Context.GenericTag("@effect/platform/HttpClient");
/** @internal */
const currentTracerDisabledWhen = exports.currentTracerDisabledWhen = /*#__PURE__*/(0, _GlobalValue.globalValue)( /*#__PURE__*/Symbol.for("@effect/platform/HttpClient/tracerDisabledWhen"), () => FiberRef.unsafeMake(_Function.constFalse));
/** @internal */
const withTracerDisabledWhen = exports.withTracerDisabledWhen = /*#__PURE__*/(0, _Function.dual)(2, (self, pred) => Effect.locally(self, currentTracerDisabledWhen, pred));
/** @internal */
const currentTracerPropagation = exports.currentTracerPropagation = /*#__PURE__*/(0, _GlobalValue.globalValue)( /*#__PURE__*/Symbol.for("@effect/platform/HttpClient/currentTracerPropagation"), () => FiberRef.unsafeMake(true));
/** @internal */
const withTracerPropagation = exports.withTracerPropagation = /*#__PURE__*/(0, _Function.dual)(2, (self, enabled) => Effect.locally(self, currentTracerPropagation, enabled));
/** @internal */
const currentFetchOptions = exports.currentFetchOptions = /*#__PURE__*/(0, _GlobalValue.globalValue)( /*#__PURE__*/Symbol.for("@effect/platform/HttpClient/currentFetchOptions"), () => FiberRef.unsafeMake({}));
/** @internal */
const withFetchOptions = exports.withFetchOptions = /*#__PURE__*/(0, _Function.dual)(2, (self, options) => Effect.locally(self, currentFetchOptions, options));
const clientProto = {
  [TypeId]: TypeId,
  pipe() {
    return (0, _Pipeable.pipeArguments)(this, arguments);
  }
};
const isClient = u => Predicate.hasProperty(u, TypeId);
/** @internal */
const make = (execute, preprocess) => {
  function client(request) {
    return execute(preprocess(request));
  }
  Object.setPrototypeOf(client, clientProto);
  client.preprocess = preprocess;
  client.execute = execute;
  return client;
};
/** @internal */
exports.make = make;
const makeDefault = f => make(effect => Effect.flatMap(effect, request => Effect.withFiberRuntime(fiber => {
  const scope = Context.unsafeGet(fiber.getFiberRef(FiberRef.currentContext), Scope.Scope);
  const controller = new AbortController();
  const addAbort = Scope.addFinalizer(scope, Effect.sync(() => controller.abort()));
  const urlResult = UrlParams.makeUrl(request.url, request.urlParams, request.hash);
  if (urlResult._tag === "Left") {
    return Effect.fail(new Error.RequestError({
      request,
      reason: "InvalidUrl",
      cause: urlResult.left
    }));
  }
  const url = urlResult.right;
  const tracerDisabled = !fiber.getFiberRef(FiberRef.currentTracerEnabled) || fiber.getFiberRef(currentTracerDisabledWhen)(request);
  if (tracerDisabled) {
    return Effect.zipRight(addAbort, f(request, url, controller.signal, fiber));
  }
  return Effect.zipRight(addAbort, Effect.useSpan(`http.client ${request.method}`, {
    kind: "client",
    captureStackTrace: false
  }, span => {
    span.attribute("http.request.method", request.method);
    span.attribute("server.address", url.origin);
    if (url.port !== "") {
      span.attribute("server.port", +url.port);
    }
    span.attribute("url.full", url.toString());
    span.attribute("url.path", url.pathname);
    span.attribute("url.scheme", url.protocol.slice(0, -1));
    const query = url.search.slice(1);
    if (query !== "") {
      span.attribute("url.query", query);
    }
    const redactedHeaderNames = fiber.getFiberRef(Headers.currentRedactedNames);
    const redactedHeaders = Headers.redact(request.headers, redactedHeaderNames);
    for (const name in redactedHeaders) {
      span.attribute(`http.request.header.${name}`, String(redactedHeaders[name]));
    }
    request = fiber.getFiberRef(currentTracerPropagation) ? internalRequest.setHeaders(request, TraceContext.toHeaders(span)) : request;
    return Effect.tap(Effect.withParentSpan(f(request, url, controller.signal, fiber), span), response => {
      span.attribute("http.response.status_code", response.status);
      const redactedHeaders = Headers.redact(response.headers, redactedHeaderNames);
      for (const name in redactedHeaders) {
        span.attribute(`http.response.header.${name}`, String(redactedHeaders[name]));
      }
    });
  }));
})), Effect.succeed);
/** @internal */
exports.makeDefault = makeDefault;
const Fetch = exports.Fetch = /*#__PURE__*/Context.GenericTag("@effect/platform/HttpClient/Fetch");
/** @internal */
const fetch = exports.fetch = /*#__PURE__*/makeDefault((request, url, signal, fiber) => {
  const context = fiber.getFiberRef(FiberRef.currentContext);
  const fetch = context.unsafeMap.get(Fetch.key) ?? globalThis.fetch;
  const options = fiber.getFiberRef(currentFetchOptions);
  const headers = new globalThis.Headers(request.headers);
  const send = body => Effect.map(Effect.tryPromise({
    try: () => fetch(url, {
      ...options,
      method: request.method,
      headers,
      body,
      duplex: request.body._tag === "Stream" ? "half" : undefined,
      signal
    }),
    catch: cause => new Error.RequestError({
      request,
      reason: "Transport",
      cause
    })
  }), response => internalResponse.fromWeb(request, response));
  if (Method.hasBody(request.method)) {
    switch (request.body._tag) {
      case "Raw":
      case "Uint8Array":
        return send(request.body.body);
      case "FormData":
        return send(request.body.formData);
      case "Stream":
        return Effect.flatMap(Stream.toReadableStreamEffect(request.body.stream), send);
    }
  }
  return send(undefined);
});
/** @internal */
const transform = exports.transform = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => make(Effect.flatMap(request => f(self.execute(Effect.succeed(request)), request)), self.preprocess));
/** @internal */
const filterStatus = exports.filterStatus = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => transform(self, (effect, request) => Effect.filterOrFail(effect, response => f(response.status), response => new Error.ResponseError({
  request,
  response,
  reason: "StatusCode",
  description: "invalid status code"
}))));
/** @internal */
const filterStatusOk = self => transform(self, (effect, request) => Effect.filterOrFail(effect, response => response.status >= 200 && response.status < 300, response => new Error.ResponseError({
  request,
  response,
  reason: "StatusCode",
  description: "non 2xx status code"
})));
/** @internal */
exports.filterStatusOk = filterStatusOk;
const fetchOk = exports.fetchOk = /*#__PURE__*/filterStatusOk(fetch);
/** @internal */
const layer = exports.layer = /*#__PURE__*/Layer.succeed(tag, fetch);
/** @internal */
const transformResponse = exports.transformResponse = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => make(request => f(self.execute(request)), self.preprocess));
/** @internal */
const catchTag = exports.catchTag = /*#__PURE__*/(0, _Function.dual)(3, (self, tag, f) => transformResponse(self, Effect.catchTag(tag, f)));
/** @internal */
const catchTags = exports.catchTags = /*#__PURE__*/(0, _Function.dual)(2, (self, cases) => transformResponse(self, Effect.catchTags(cases)));
/** @internal */
const catchAll = exports.catchAll = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => transformResponse(self, Effect.catchAll(f)));
/** @internal */
const filterOrElse = exports.filterOrElse = /*#__PURE__*/(0, _Function.dual)(3, (self, f, orElse) => transformResponse(self, Effect.filterOrElse(f, orElse)));
/** @internal */
const filterOrFail = exports.filterOrFail = /*#__PURE__*/(0, _Function.dual)(3, (self, f, orFailWith) => transformResponse(self, Effect.filterOrFail(f, orFailWith)));
/** @internal */
const map = exports.map = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => transformResponse(self, Effect.map(f)));
/** @internal */
const mapEffect = exports.mapEffect = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => transformResponse(self, Effect.flatMap(f)));
/** @internal */
const scoped = self => transformResponse(self, Effect.scoped);
/** @internal */
exports.scoped = scoped;
const mapEffectScoped = exports.mapEffectScoped = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => scoped(mapEffect(self, f)));
/** @internal */
const mapRequest = exports.mapRequest = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => make(self.execute, request => Effect.map(self.preprocess(request), f)));
/** @internal */
const mapRequestEffect = exports.mapRequestEffect = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => make(self.execute, request => Effect.flatMap(self.preprocess(request), f)));
/** @internal */
const mapInputRequest = exports.mapInputRequest = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => make(self.execute, request => self.preprocess(f(request))));
/** @internal */
const mapInputRequestEffect = exports.mapInputRequestEffect = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => make(self.execute, request => Effect.flatMap(f(request), self.preprocess)));
/** @internal */
const retry = exports.retry = /*#__PURE__*/(0, _Function.dual)(2, (self, policy) => transformResponse(self, Effect.retry(policy)));
/** @internal */
const schemaFunction = exports.schemaFunction = /*#__PURE__*/(0, _Function.dual)(args => isClient(args[0]), (self, schema, options) => {
  const encode = Schema.encode(schema, options);
  return request => a => Effect.flatMap(Effect.tryMap(encode(a), {
    try: body => new TextEncoder().encode(JSON.stringify(body)),
    catch: cause => new Error.RequestError({
      request,
      reason: "Encode",
      cause
    })
  }), body => self(internalRequest.setBody(request, internalBody.uint8Array(body, "application/json"))));
});
/** @internal */
const tap = exports.tap = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => transformResponse(self, Effect.tap(f)));
/** @internal */
const tapRequest = exports.tapRequest = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => make(self.execute, request => Effect.tap(self.preprocess(request), f)));
/** @internal */
const withCookiesRef = exports.withCookiesRef = /*#__PURE__*/(0, _Function.dual)(2, (self, ref) => make(request => Effect.tap(self.execute(request), response => Ref.update(ref, cookies => Cookies.merge(cookies, response.cookies))), request => Effect.flatMap(self.preprocess(request), request => Effect.map(Ref.get(ref), cookies => Cookies.isEmpty(cookies) ? request : internalRequest.setHeader(request, "cookie", Cookies.toCookieHeader(cookies))))));
/** @internal */
const followRedirects = exports.followRedirects = /*#__PURE__*/(0, _Function.dual)(args => isClient(args[0]), (self, maxRedirects) => make(request => {
  const loop = (request, redirects) => Effect.flatMap(self.execute(Effect.succeed(request)), response => response.status >= 300 && response.status < 400 && response.headers.location && redirects < (maxRedirects ?? 10) ? loop(internalRequest.setUrl(request, response.headers.location), redirects + 1) : Effect.succeed(response));
  return Effect.flatMap(request, request => loop(request, 0));
}, self.preprocess));
//# sourceMappingURL=httpClient.js.map