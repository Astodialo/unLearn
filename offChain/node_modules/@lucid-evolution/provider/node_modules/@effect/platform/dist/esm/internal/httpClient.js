import * as Schema from "@effect/schema/Schema";
import * as Context from "effect/Context";
import * as Effect from "effect/Effect";
import * as FiberRef from "effect/FiberRef";
import { constFalse, dual } from "effect/Function";
import { globalValue } from "effect/GlobalValue";
import * as Layer from "effect/Layer";
import { pipeArguments } from "effect/Pipeable";
import * as Predicate from "effect/Predicate";
import * as Ref from "effect/Ref";
import * as Scope from "effect/Scope";
import * as Stream from "effect/Stream";
import * as Cookies from "../Cookies.js";
import * as Headers from "../Headers.js";
import * as Error from "../HttpClientError.js";
import * as Method from "../HttpMethod.js";
import * as TraceContext from "../HttpTraceContext.js";
import * as UrlParams from "../UrlParams.js";
import * as internalBody from "./httpBody.js";
import * as internalRequest from "./httpClientRequest.js";
import * as internalResponse from "./httpClientResponse.js";
/** @internal */
export const TypeId = /*#__PURE__*/Symbol.for("@effect/platform/HttpClient");
/** @internal */
export const tag = /*#__PURE__*/Context.GenericTag("@effect/platform/HttpClient");
/** @internal */
export const currentTracerDisabledWhen = /*#__PURE__*/globalValue( /*#__PURE__*/Symbol.for("@effect/platform/HttpClient/tracerDisabledWhen"), () => FiberRef.unsafeMake(constFalse));
/** @internal */
export const withTracerDisabledWhen = /*#__PURE__*/dual(2, (self, pred) => Effect.locally(self, currentTracerDisabledWhen, pred));
/** @internal */
export const currentTracerPropagation = /*#__PURE__*/globalValue( /*#__PURE__*/Symbol.for("@effect/platform/HttpClient/currentTracerPropagation"), () => FiberRef.unsafeMake(true));
/** @internal */
export const withTracerPropagation = /*#__PURE__*/dual(2, (self, enabled) => Effect.locally(self, currentTracerPropagation, enabled));
/** @internal */
export const currentFetchOptions = /*#__PURE__*/globalValue( /*#__PURE__*/Symbol.for("@effect/platform/HttpClient/currentFetchOptions"), () => FiberRef.unsafeMake({}));
/** @internal */
export const withFetchOptions = /*#__PURE__*/dual(2, (self, options) => Effect.locally(self, currentFetchOptions, options));
const clientProto = {
  [TypeId]: TypeId,
  pipe() {
    return pipeArguments(this, arguments);
  }
};
const isClient = u => Predicate.hasProperty(u, TypeId);
/** @internal */
export const make = (execute, preprocess) => {
  function client(request) {
    return execute(preprocess(request));
  }
  Object.setPrototypeOf(client, clientProto);
  client.preprocess = preprocess;
  client.execute = execute;
  return client;
};
/** @internal */
export const makeDefault = f => make(effect => Effect.flatMap(effect, request => Effect.withFiberRuntime(fiber => {
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
export const Fetch = /*#__PURE__*/Context.GenericTag("@effect/platform/HttpClient/Fetch");
/** @internal */
export const fetch = /*#__PURE__*/makeDefault((request, url, signal, fiber) => {
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
export const transform = /*#__PURE__*/dual(2, (self, f) => make(Effect.flatMap(request => f(self.execute(Effect.succeed(request)), request)), self.preprocess));
/** @internal */
export const filterStatus = /*#__PURE__*/dual(2, (self, f) => transform(self, (effect, request) => Effect.filterOrFail(effect, response => f(response.status), response => new Error.ResponseError({
  request,
  response,
  reason: "StatusCode",
  description: "invalid status code"
}))));
/** @internal */
export const filterStatusOk = self => transform(self, (effect, request) => Effect.filterOrFail(effect, response => response.status >= 200 && response.status < 300, response => new Error.ResponseError({
  request,
  response,
  reason: "StatusCode",
  description: "non 2xx status code"
})));
/** @internal */
export const fetchOk = /*#__PURE__*/filterStatusOk(fetch);
/** @internal */
export const layer = /*#__PURE__*/Layer.succeed(tag, fetch);
/** @internal */
export const transformResponse = /*#__PURE__*/dual(2, (self, f) => make(request => f(self.execute(request)), self.preprocess));
/** @internal */
export const catchTag = /*#__PURE__*/dual(3, (self, tag, f) => transformResponse(self, Effect.catchTag(tag, f)));
/** @internal */
export const catchTags = /*#__PURE__*/dual(2, (self, cases) => transformResponse(self, Effect.catchTags(cases)));
/** @internal */
export const catchAll = /*#__PURE__*/dual(2, (self, f) => transformResponse(self, Effect.catchAll(f)));
/** @internal */
export const filterOrElse = /*#__PURE__*/dual(3, (self, f, orElse) => transformResponse(self, Effect.filterOrElse(f, orElse)));
/** @internal */
export const filterOrFail = /*#__PURE__*/dual(3, (self, f, orFailWith) => transformResponse(self, Effect.filterOrFail(f, orFailWith)));
/** @internal */
export const map = /*#__PURE__*/dual(2, (self, f) => transformResponse(self, Effect.map(f)));
/** @internal */
export const mapEffect = /*#__PURE__*/dual(2, (self, f) => transformResponse(self, Effect.flatMap(f)));
/** @internal */
export const scoped = self => transformResponse(self, Effect.scoped);
/** @internal */
export const mapEffectScoped = /*#__PURE__*/dual(2, (self, f) => scoped(mapEffect(self, f)));
/** @internal */
export const mapRequest = /*#__PURE__*/dual(2, (self, f) => make(self.execute, request => Effect.map(self.preprocess(request), f)));
/** @internal */
export const mapRequestEffect = /*#__PURE__*/dual(2, (self, f) => make(self.execute, request => Effect.flatMap(self.preprocess(request), f)));
/** @internal */
export const mapInputRequest = /*#__PURE__*/dual(2, (self, f) => make(self.execute, request => self.preprocess(f(request))));
/** @internal */
export const mapInputRequestEffect = /*#__PURE__*/dual(2, (self, f) => make(self.execute, request => Effect.flatMap(f(request), self.preprocess)));
/** @internal */
export const retry = /*#__PURE__*/dual(2, (self, policy) => transformResponse(self, Effect.retry(policy)));
/** @internal */
export const schemaFunction = /*#__PURE__*/dual(args => isClient(args[0]), (self, schema, options) => {
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
export const tap = /*#__PURE__*/dual(2, (self, f) => transformResponse(self, Effect.tap(f)));
/** @internal */
export const tapRequest = /*#__PURE__*/dual(2, (self, f) => make(self.execute, request => Effect.tap(self.preprocess(request), f)));
/** @internal */
export const withCookiesRef = /*#__PURE__*/dual(2, (self, ref) => make(request => Effect.tap(self.execute(request), response => Ref.update(ref, cookies => Cookies.merge(cookies, response.cookies))), request => Effect.flatMap(self.preprocess(request), request => Effect.map(Ref.get(ref), cookies => Cookies.isEmpty(cookies) ? request : internalRequest.setHeader(request, "cookie", Cookies.toCookieHeader(cookies))))));
/** @internal */
export const followRedirects = /*#__PURE__*/dual(args => isClient(args[0]), (self, maxRedirects) => make(request => {
  const loop = (request, redirects) => Effect.flatMap(self.execute(Effect.succeed(request)), response => response.status >= 300 && response.status < 400 && response.headers.location && redirects < (maxRedirects ?? 10) ? loop(internalRequest.setUrl(request, response.headers.location), redirects + 1) : Effect.succeed(response));
  return Effect.flatMap(request, request => loop(request, 0));
}, self.preprocess));
//# sourceMappingURL=httpClient.js.map