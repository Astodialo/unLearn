"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.matchStatusScoped = exports.matchStatus = exports.json = exports.fromWeb = exports.formData = exports.arrayBuffer = exports.TypeId = void 0;
Object.defineProperty(exports, "schemaBodyJson", {
  enumerable: true,
  get: function () {
    return _HttpIncomingMessage.schemaBodyJson;
  }
});
Object.defineProperty(exports, "schemaBodyJsonScoped", {
  enumerable: true,
  get: function () {
    return _HttpIncomingMessage.schemaBodyJsonScoped;
  }
});
Object.defineProperty(exports, "schemaBodyUrlParams", {
  enumerable: true,
  get: function () {
    return _HttpIncomingMessage.schemaBodyUrlParams;
  }
});
Object.defineProperty(exports, "schemaBodyUrlParamsScoped", {
  enumerable: true,
  get: function () {
    return _HttpIncomingMessage.schemaBodyUrlParamsScoped;
  }
});
Object.defineProperty(exports, "schemaHeaders", {
  enumerable: true,
  get: function () {
    return _HttpIncomingMessage.schemaHeaders;
  }
});
Object.defineProperty(exports, "schemaHeadersScoped", {
  enumerable: true,
  get: function () {
    return _HttpIncomingMessage.schemaHeadersScoped;
  }
});
exports.void = exports.urlParamsBody = exports.text = exports.stream = exports.schemaNoBodyScoped = exports.schemaNoBody = exports.schemaJsonScoped = exports.schemaJson = void 0;
var internal = _interopRequireWildcard(require("./internal/httpClientResponse.js"));
var _HttpIncomingMessage = require("./HttpIncomingMessage.js");
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 * @category type ids
 */
const TypeId = exports.TypeId = internal.TypeId;
/**
 * @since 1.0.0
 * @category constructors
 */
const fromWeb = exports.fromWeb = internal.fromWeb;
/**
 * @since 1.0.0
 * @category schema
 */
const schemaJson = exports.schemaJson = internal.schemaJson;
/**
 * @since 1.0.0
 * @category schema
 */
const schemaNoBody = exports.schemaNoBody = internal.schemaNoBody;
/**
 * @since 1.0.0
 * @category accessors
 */
const arrayBuffer = exports.arrayBuffer = internal.arrayBuffer;
/**
 * @since 1.0.0
 * @category accessors
 */
const formData = exports.formData = internal.formData;
/**
 * @since 1.0.0
 * @category accessors
 */
const json = exports.json = internal.json;
const void_ = exports.void = internal.void_;
/**
 * @since 1.0.0
 * @category accessors
 */
const stream = exports.stream = internal.stream;
/**
 * @since 1.0.0
 * @category accessors
 */
const text = exports.text = internal.text;
/**
 * @since 1.0.0
 * @category accessors
 */
const urlParamsBody = exports.urlParamsBody = internal.urlParamsBody;
/**
 * @since 1.0.0
 * @category schema
 */
const schemaJsonScoped = exports.schemaJsonScoped = internal.schemaJsonScoped;
/**
 * @since 1.0.0
 * @category schema
 */
const schemaNoBodyScoped = exports.schemaNoBodyScoped = internal.schemaNoBodyScoped;
/**
 * @since 1.0.0
 * @category pattern matching
 */
const matchStatus = exports.matchStatus = internal.matchStatus;
/**
 * @since 1.0.0
 * @category pattern matching
 */
const matchStatusScoped = exports.matchStatusScoped = internal.matchStatusScoped;
//# sourceMappingURL=HttpClientResponse.js.map