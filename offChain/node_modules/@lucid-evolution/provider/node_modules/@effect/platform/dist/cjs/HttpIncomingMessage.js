"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.withMaxBodySize = exports.schemaHeadersScoped = exports.schemaHeaders = exports.schemaBodyUrlParamsScoped = exports.schemaBodyUrlParams = exports.schemaBodyJsonScoped = exports.schemaBodyJson = exports.maxBodySize = exports.inspect = exports.TypeId = void 0;
var Schema = _interopRequireWildcard(require("@effect/schema/Schema"));
var Effect = _interopRequireWildcard(require("effect/Effect"));
var FiberRef = _interopRequireWildcard(require("effect/FiberRef"));
var _Function = require("effect/Function");
var Global = _interopRequireWildcard(require("effect/GlobalValue"));
var Option = _interopRequireWildcard(require("effect/Option"));
var FileSystem = _interopRequireWildcard(require("./FileSystem.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 * @category type ids
 */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("@effect/platform/HttpIncomingMessage");
/**
 * @since 1.0.0
 * @category schema
 */
const schemaBodyJson = (schema, options) => {
  const parse = Schema.decodeUnknown(schema, options);
  return self => Effect.flatMap(self.json, parse);
};
/**
 * @since 1.0.0
 * @category schema
 */
exports.schemaBodyJson = schemaBodyJson;
const schemaBodyJsonScoped = (schema, options) => {
  const decode = schemaBodyJson(schema, options);
  return effect => Effect.scoped(Effect.flatMap(effect, decode));
};
/**
 * @since 1.0.0
 * @category schema
 */
exports.schemaBodyJsonScoped = schemaBodyJsonScoped;
const schemaBodyUrlParams = (schema, options) => {
  const parse = Schema.decodeUnknown(schema, options);
  return self => Effect.flatMap(self.urlParamsBody, _ => parse(Object.fromEntries(_)));
};
/**
 * @since 1.0.0
 * @category schema
 */
exports.schemaBodyUrlParams = schemaBodyUrlParams;
const schemaBodyUrlParamsScoped = (schema, options) => {
  const decode = schemaBodyUrlParams(schema, options);
  return effect => Effect.scoped(Effect.flatMap(effect, decode));
};
/**
 * @since 1.0.0
 * @category schema
 */
exports.schemaBodyUrlParamsScoped = schemaBodyUrlParamsScoped;
const schemaHeaders = (schema, options) => {
  const parse = Schema.decodeUnknown(schema, options);
  return self => parse(self.headers);
};
/**
 * @since 1.0.0
 * @category schema
 */
exports.schemaHeaders = schemaHeaders;
const schemaHeadersScoped = (schema, options) => {
  const decode = schemaHeaders(schema, options);
  return effect => Effect.scoped(Effect.flatMap(effect, decode));
};
/**
 * @since 1.0.0
 * @category fiber refs
 */
exports.schemaHeadersScoped = schemaHeadersScoped;
const maxBodySize = exports.maxBodySize = /*#__PURE__*/Global.globalValue("@effect/platform/HttpIncomingMessage/maxBodySize", () => FiberRef.unsafeMake(Option.none()));
/**
 * @since 1.0.0
 * @category fiber refs
 */
const withMaxBodySize = exports.withMaxBodySize = /*#__PURE__*/(0, _Function.dual)(2, (effect, size) => Effect.locally(effect, maxBodySize, Option.map(size, FileSystem.Size)));
/**
 * @since 1.0.0
 */
const inspect = (self, that) => {
  const contentType = self.headers["content-type"] ?? "";
  let body;
  if (contentType.includes("application/json")) {
    try {
      body = Effect.runSync(self.json);
    } catch (_) {
      //
    }
  } else if (contentType.includes("text/") || contentType.includes("urlencoded")) {
    try {
      body = Effect.runSync(self.text);
    } catch (_) {
      //
    }
  }
  const obj = {
    ...that,
    headers: self.headers,
    remoteAddress: self.remoteAddress.toJSON()
  };
  if (body !== undefined) {
    obj.body = body;
  }
  return obj;
};
exports.inspect = inspect;
//# sourceMappingURL=HttpIncomingMessage.js.map