"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.urlParamsBody = exports.updateUrl = exports.unsafeJsonBody = exports.uint8ArrayBody = exports.textBody = exports.streamBody = exports.setUrlParams = exports.setUrlParam = exports.setUrl = exports.setMethod = exports.setHeaders = exports.setHeader = exports.setHash = exports.setBody = exports.schemaBody = exports.removeHash = exports.put = exports.prependUrl = exports.post = exports.patch = exports.options = exports.modify = exports.make = exports.jsonBody = exports.isClientRequest = exports.head = exports.get = exports.formDataBody = exports.fileWebBody = exports.fileBody = exports.empty = exports.del = exports.clientTag = exports.bearerToken = exports.basicAuth = exports.appendUrlParams = exports.appendUrlParam = exports.appendUrl = exports.acceptJson = exports.accept = exports.TypeId = void 0;
var Context = _interopRequireWildcard(require("effect/Context"));
var Effect = _interopRequireWildcard(require("effect/Effect"));
var Effectable = _interopRequireWildcard(require("effect/Effectable"));
var _Function = require("effect/Function");
var Inspectable = _interopRequireWildcard(require("effect/Inspectable"));
var Option = _interopRequireWildcard(require("effect/Option"));
var Headers = _interopRequireWildcard(require("../Headers.js"));
var UrlParams = _interopRequireWildcard(require("../UrlParams.js"));
var internalBody = _interopRequireWildcard(require("./httpBody.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/** @internal */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("@effect/platform/HttpClientRequest");
/** @internal */
const clientTag = exports.clientTag = /*#__PURE__*/Context.GenericTag("@effect/platform/HttpClient");
const Proto = {
  [TypeId]: TypeId,
  ...Effectable.CommitPrototype,
  ...Inspectable.BaseProto,
  commit() {
    return Effect.flatMap(clientTag, client => client(this));
  },
  toJSON() {
    return {
      _id: "@effect/platform/HttpClientRequest",
      method: this.method,
      url: this.url,
      urlParams: this.urlParams,
      hash: this.hash,
      headers: this.headers,
      body: this.body.toJSON()
    };
  }
};
function makeInternal(method, url, urlParams, hash, headers, body) {
  const self = Object.create(Proto);
  self.method = method;
  self.url = url;
  self.urlParams = urlParams;
  self.hash = hash;
  self.headers = headers;
  self.body = body;
  return self;
}
/** @internal */
const isClientRequest = u => typeof u === "object" && u !== null && TypeId in u;
/** @internal */
exports.isClientRequest = isClientRequest;
const empty = exports.empty = /*#__PURE__*/makeInternal("GET", "", UrlParams.empty, /*#__PURE__*/Option.none(), Headers.empty, internalBody.empty);
/** @internal */
const make = method => (url, options) => modify(empty, {
  method,
  url,
  ...(options ?? undefined)
});
/** @internal */
exports.make = make;
const get = exports.get = /*#__PURE__*/make("GET");
/** @internal */
const post = exports.post = /*#__PURE__*/make("POST");
/** @internal */
const put = exports.put = /*#__PURE__*/make("PUT");
/** @internal */
const patch = exports.patch = /*#__PURE__*/make("PATCH");
/** @internal */
const del = exports.del = /*#__PURE__*/make("DELETE");
/** @internal */
const head = exports.head = /*#__PURE__*/make("HEAD");
/** @internal */
const options = exports.options = /*#__PURE__*/make("OPTIONS");
/** @internal */
const modify = exports.modify = /*#__PURE__*/(0, _Function.dual)(2, (self, options) => {
  let result = self;
  if (options.method) {
    result = setMethod(result, options.method);
  }
  if (options.url) {
    result = setUrl(result, options.url);
  }
  if (options.headers) {
    result = setHeaders(result, options.headers);
  }
  if (options.urlParams) {
    result = setUrlParams(result, options.urlParams);
  }
  if (options.hash) {
    result = setHash(result, options.hash);
  }
  if (options.body) {
    result = setBody(result, options.body);
  }
  if (options.accept) {
    result = accept(result, options.accept);
  }
  if (options.acceptJson) {
    result = acceptJson(result);
  }
  return result;
});
/** @internal */
const setHeader = exports.setHeader = /*#__PURE__*/(0, _Function.dual)(3, (self, key, value) => makeInternal(self.method, self.url, self.urlParams, self.hash, Headers.set(self.headers, key, value), self.body));
/** @internal */
const setHeaders = exports.setHeaders = /*#__PURE__*/(0, _Function.dual)(2, (self, input) => makeInternal(self.method, self.url, self.urlParams, self.hash, Headers.setAll(self.headers, input), self.body));
/** @internal */
const basicAuth = exports.basicAuth = /*#__PURE__*/(0, _Function.dual)(3, (self, username, password) => setHeader(self, "Authorization", `Basic ${btoa(`${username}:${password}`)}`));
/** @internal */
const bearerToken = exports.bearerToken = /*#__PURE__*/(0, _Function.dual)(2, (self, token) => setHeader(self, "Authorization", `Bearer ${token}`));
/** @internal */
const accept = exports.accept = /*#__PURE__*/(0, _Function.dual)(2, (self, mediaType) => setHeader(self, "Accept", mediaType));
/** @internal */
const acceptJson = exports.acceptJson = /*#__PURE__*/accept("application/json");
/** @internal */
const setMethod = exports.setMethod = /*#__PURE__*/(0, _Function.dual)(2, (self, method) => makeInternal(method, self.url, self.urlParams, self.hash, self.headers, self.body));
/** @internal */
const setUrl = exports.setUrl = /*#__PURE__*/(0, _Function.dual)(2, (self, url) => {
  if (typeof url === "string") {
    return makeInternal(self.method, url, self.urlParams, self.hash, self.headers, self.body);
  }
  const clone = new URL(url.toString());
  const urlParams = UrlParams.fromInput(clone.searchParams);
  const hash = clone.hash ? Option.some(clone.hash.slice(1)) : Option.none();
  clone.search = "";
  clone.hash = "";
  return makeInternal(self.method, clone.toString(), urlParams, hash, self.headers, self.body);
});
/** @internal */
const appendUrl = exports.appendUrl = /*#__PURE__*/(0, _Function.dual)(2, (self, url) => makeInternal(self.method, self.url.endsWith("/") && url.startsWith("/") ? self.url + url.slice(1) : self.url + url, self.urlParams, self.hash, self.headers, self.body));
/** @internal */
const prependUrl = exports.prependUrl = /*#__PURE__*/(0, _Function.dual)(2, (self, url) => makeInternal(self.method, url.endsWith("/") && self.url.startsWith("/") ? url + self.url.slice(1) : url + self.url, self.urlParams, self.hash, self.headers, self.body));
/** @internal */
const updateUrl = exports.updateUrl = /*#__PURE__*/(0, _Function.dual)(2, (self, f) => makeInternal(self.method, f(self.url), self.urlParams, self.hash, self.headers, self.body));
/** @internal */
const appendUrlParam = exports.appendUrlParam = /*#__PURE__*/(0, _Function.dual)(3, (self, key, value) => makeInternal(self.method, self.url, UrlParams.append(self.urlParams, key, value), self.hash, self.headers, self.body));
/** @internal */
const appendUrlParams = exports.appendUrlParams = /*#__PURE__*/(0, _Function.dual)(2, (self, input) => makeInternal(self.method, self.url, UrlParams.appendAll(self.urlParams, input), self.hash, self.headers, self.body));
/** @internal */
const setUrlParam = exports.setUrlParam = /*#__PURE__*/(0, _Function.dual)(3, (self, key, value) => makeInternal(self.method, self.url, UrlParams.set(self.urlParams, key, value), self.hash, self.headers, self.body));
/** @internal */
const setUrlParams = exports.setUrlParams = /*#__PURE__*/(0, _Function.dual)(2, (self, input) => makeInternal(self.method, self.url, UrlParams.setAll(self.urlParams, input), self.hash, self.headers, self.body));
/** @internal */
const setHash = exports.setHash = /*#__PURE__*/(0, _Function.dual)(2, (self, hash) => makeInternal(self.method, self.url, self.urlParams, Option.some(hash), self.headers, self.body));
/** @internal */
const removeHash = self => makeInternal(self.method, self.url, self.urlParams, Option.none(), self.headers, self.body);
/** @internal */
exports.removeHash = removeHash;
const setBody = exports.setBody = /*#__PURE__*/(0, _Function.dual)(2, (self, body) => {
  let headers = self.headers;
  if (body._tag === "Empty") {
    headers = Headers.remove(Headers.remove(headers, "Content-Type"), "Content-length");
  } else {
    const contentType = body.contentType;
    if (contentType) {
      headers = Headers.set(headers, "content-type", contentType);
    }
    const contentLength = body.contentLength;
    if (contentLength) {
      headers = Headers.set(headers, "content-length", contentLength.toString());
    }
  }
  return makeInternal(self.method, self.url, self.urlParams, self.hash, headers, body);
});
/** @internal */
const uint8ArrayBody = exports.uint8ArrayBody = /*#__PURE__*/(0, _Function.dual)(args => isClientRequest(args[0]), (self, body, contentType = "application/octet-stream") => setBody(self, internalBody.uint8Array(body, contentType)));
/** @internal */
const textBody = exports.textBody = /*#__PURE__*/(0, _Function.dual)(args => isClientRequest(args[0]), (self, body, contentType = "text/plain") => setBody(self, internalBody.text(body, contentType)));
/** @internal */
const jsonBody = exports.jsonBody = /*#__PURE__*/(0, _Function.dual)(2, (self, body) => Effect.map(internalBody.json(body), body => setBody(self, body)));
/** @internal */
const unsafeJsonBody = exports.unsafeJsonBody = /*#__PURE__*/(0, _Function.dual)(2, (self, body) => setBody(self, internalBody.unsafeJson(body)));
/** @internal */
const fileBody = exports.fileBody = /*#__PURE__*/(0, _Function.dual)(args => isClientRequest(args[0]), (self, path, options) => Effect.map(internalBody.file(path, options), body => setBody(self, body)));
/** @internal */
const fileWebBody = exports.fileWebBody = /*#__PURE__*/(0, _Function.dual)(2, (self, file) => setBody(self, internalBody.fileWeb(file)));
/** @internal */
const schemaBody = (schema, options) => {
  const encode = internalBody.jsonSchema(schema, options);
  return (0, _Function.dual)(2, (self, body) => Effect.map(encode(body), body => setBody(self, body)));
};
/** @internal */
exports.schemaBody = schemaBody;
const urlParamsBody = exports.urlParamsBody = /*#__PURE__*/(0, _Function.dual)(2, (self, body) => setBody(self, internalBody.text(UrlParams.toString(UrlParams.fromInput(body)), "application/x-www-form-urlencoded")));
/** @internal */
const formDataBody = exports.formDataBody = /*#__PURE__*/(0, _Function.dual)(2, (self, body) => setBody(self, internalBody.formData(body)));
/** @internal */
const streamBody = exports.streamBody = /*#__PURE__*/(0, _Function.dual)(args => isClientRequest(args[0]), (self, body, {
  contentLength,
  contentType = "application/octet-stream"
} = {}) => setBody(self, internalBody.stream(body, contentType, contentLength)));
//# sourceMappingURL=httpClientRequest.js.map