"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.void_ = exports.urlParamsBody = exports.text = exports.stream = exports.schemaNoBodyScoped = exports.schemaNoBody = exports.schemaJsonScoped = exports.schemaJson = exports.matchStatusScoped = exports.matchStatus = exports.json = exports.fromWeb = exports.formData = exports.arrayBuffer = exports.TypeId = void 0;
var Schema = _interopRequireWildcard(require("@effect/schema/Schema"));
var Effect = _interopRequireWildcard(require("effect/Effect"));
var _Function = require("effect/Function");
var Inspectable = _interopRequireWildcard(require("effect/Inspectable"));
var Option = _interopRequireWildcard(require("effect/Option"));
var Stream = _interopRequireWildcard(require("effect/Stream"));
var Cookies = _interopRequireWildcard(require("../Cookies.js"));
var Headers = _interopRequireWildcard(require("../Headers.js"));
var Error = _interopRequireWildcard(require("../HttpClientError.js"));
var IncomingMessage = _interopRequireWildcard(require("../HttpIncomingMessage.js"));
var UrlParams = _interopRequireWildcard(require("../UrlParams.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("@effect/platform/HttpClientResponse");
/** @internal */
const fromWeb = (request, source) => new ClientResponseImpl(request, source);
exports.fromWeb = fromWeb;
class ClientResponseImpl extends Inspectable.Class {
  request;
  source;
  [IncomingMessage.TypeId];
  [TypeId];
  constructor(request, source) {
    super();
    this.request = request;
    this.source = source;
    this[IncomingMessage.TypeId] = IncomingMessage.TypeId;
    this[TypeId] = TypeId;
  }
  toJSON() {
    return IncomingMessage.inspect(this, {
      _id: "@effect/platform/HttpClientResponse",
      request: this.request.toJSON(),
      status: this.status
    });
  }
  get status() {
    return this.source.status;
  }
  get headers() {
    return Headers.fromInput(this.source.headers);
  }
  cachedCookies;
  get cookies() {
    if (this.cachedCookies) {
      return this.cachedCookies;
    }
    return this.cachedCookies = Cookies.fromSetCookie(this.source.headers.getSetCookie());
  }
  get remoteAddress() {
    return Option.none();
  }
  get stream() {
    return this.source.body ? Stream.fromReadableStream(() => this.source.body, cause => new Error.ResponseError({
      request: this.request,
      response: this,
      reason: "Decode",
      cause
    })) : Stream.fail(new Error.ResponseError({
      request: this.request,
      response: this,
      reason: "EmptyBody",
      description: "can not create stream from empty body"
    }));
  }
  get json() {
    return Effect.tryMap(this.text, {
      try: text => text === "" ? null : JSON.parse(text),
      catch: cause => new Error.ResponseError({
        request: this.request,
        response: this,
        reason: "Decode",
        cause
      })
    });
  }
  textBody;
  get text() {
    return this.textBody ??= Effect.tryPromise({
      try: () => this.source.text(),
      catch: cause => new Error.ResponseError({
        request: this.request,
        response: this,
        reason: "Decode",
        cause
      })
    }).pipe(Effect.cached, Effect.runSync);
  }
  get urlParamsBody() {
    return Effect.flatMap(this.text, _ => Effect.try({
      try: () => UrlParams.fromInput(new URLSearchParams(_)),
      catch: cause => new Error.ResponseError({
        request: this.request,
        response: this,
        reason: "Decode",
        cause
      })
    }));
  }
  formDataBody;
  get formData() {
    return this.formDataBody ??= Effect.tryPromise({
      try: () => this.source.formData(),
      catch: cause => new Error.ResponseError({
        request: this.request,
        response: this,
        reason: "Decode",
        cause
      })
    }).pipe(Effect.cached, Effect.runSync);
  }
  arrayBufferBody;
  get arrayBuffer() {
    return this.arrayBufferBody ??= Effect.tryPromise({
      try: () => this.source.arrayBuffer(),
      catch: cause => new Error.ResponseError({
        request: this.request,
        response: this,
        reason: "Decode",
        cause
      })
    }).pipe(Effect.cached, Effect.runSync);
  }
}
/** @internal */
const schemaJson = (schema, options) => {
  const parse = Schema.decodeUnknown(schema, options);
  return self => Effect.flatMap(self.json, body => parse({
    status: self.status,
    headers: self.headers,
    body
  }));
};
/** @internal */
exports.schemaJson = schemaJson;
const schemaNoBody = (schema, options) => {
  const parse = Schema.decodeUnknown(schema, options);
  return self => parse({
    status: self.status,
    headers: self.headers
  });
};
/** @internal */
exports.schemaNoBody = schemaNoBody;
const arrayBuffer = effect => Effect.scoped(Effect.flatMap(effect, _ => _.arrayBuffer));
/** @internal */
exports.arrayBuffer = arrayBuffer;
const text = effect => Effect.scoped(Effect.flatMap(effect, _ => _.text));
/** @internal */
exports.text = text;
const json = effect => Effect.scoped(Effect.flatMap(effect, _ => _.json));
/** @internal */
exports.json = json;
const urlParamsBody = effect => Effect.scoped(Effect.flatMap(effect, _ => _.urlParamsBody));
/** @internal */
exports.urlParamsBody = urlParamsBody;
const formData = effect => Effect.scoped(Effect.flatMap(effect, _ => _.formData));
/** @internal */
exports.formData = formData;
const void_ = effect => Effect.scoped(Effect.asVoid(effect));
/** @internal */
exports.void_ = void_;
const stream = effect => Stream.unwrapScoped(Effect.map(effect, _ => _.stream));
/** @internal */
exports.stream = stream;
const schemaJsonScoped = (schema, options) => {
  const decode = schemaJson(schema, options);
  return effect => Effect.scoped(Effect.flatMap(effect, decode));
};
/** @internal */
exports.schemaJsonScoped = schemaJsonScoped;
const schemaNoBodyScoped = (schema, options) => {
  const decode = schemaNoBody(schema, options);
  return effect => Effect.scoped(Effect.flatMap(effect, decode));
};
/** @internal */
exports.schemaNoBodyScoped = schemaNoBodyScoped;
const matchStatus = exports.matchStatus = /*#__PURE__*/(0, _Function.dual)(2, (self, cases) => {
  const status = self.status;
  if (cases[status]) {
    return cases[status](self);
  } else if (status >= 200 && status < 300 && cases["2xx"]) {
    return cases["2xx"](self);
  } else if (status >= 300 && status < 400 && cases["3xx"]) {
    return cases["3xx"](self);
  } else if (status >= 400 && status < 500 && cases["4xx"]) {
    return cases["4xx"](self);
  } else if (status >= 500 && status < 600 && cases["5xx"]) {
    return cases["5xx"](self);
  }
  return cases.orElse(self);
});
/** @internal */
const matchStatusScoped = exports.matchStatusScoped = /*#__PURE__*/(0, _Function.dual)(2, (self, cases) => Effect.scoped(Effect.flatMap(self, matchStatus(cases))));
//# sourceMappingURL=httpClientResponse.js.map