"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.fromWeb = exports.TypeId = exports.ParsedSearchParams = exports.HttpServerRequest = void 0;
Object.defineProperty(exports, "maxBodySize", {
  enumerable: true,
  get: function () {
    return _HttpIncomingMessage.maxBodySize;
  }
});
exports.upgradeChannel = exports.upgrade = exports.toURL = exports.searchParamsFromURL = exports.schemaSearchParams = exports.schemaHeaders = exports.schemaCookies = exports.schemaBodyUrlParams = exports.schemaBodyMultipart = exports.schemaBodyJson = exports.schemaBodyFormJson = exports.schemaBodyForm = exports.persistedMultipart = void 0;
var internal = _interopRequireWildcard(require("./internal/httpServerRequest.js"));
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
 * @category context
 */
const HttpServerRequest = exports.HttpServerRequest = internal.serverRequestTag;
/**
 * @since 1.0.0
 * @category search params
 */
const ParsedSearchParams = exports.ParsedSearchParams = internal.parsedSearchParamsTag;
/**
 * @since 1.0.0
 * @category search params
 */
const searchParamsFromURL = exports.searchParamsFromURL = internal.searchParamsFromURL;
/**
 * @since 1.0.0
 * @category accessors
 */
const persistedMultipart = exports.persistedMultipart = internal.multipartPersisted;
/**
 * @since 1.0.0
 * @category accessors
 */
const upgrade = exports.upgrade = internal.upgrade;
/**
 * @since 1.0.0
 * @category accessors
 */
const upgradeChannel = exports.upgradeChannel = internal.upgradeChannel;
/**
 * @since 1.0.0
 * @category schema
 */
const schemaCookies = exports.schemaCookies = internal.schemaCookies;
/**
 * @since 1.0.0
 * @category schema
 */
const schemaHeaders = exports.schemaHeaders = internal.schemaHeaders;
/**
 * @since 1.0.0
 * @category schema
 */
const schemaSearchParams = exports.schemaSearchParams = internal.schemaSearchParams;
/**
 * @since 1.0.0
 * @category schema
 */
const schemaBodyJson = exports.schemaBodyJson = internal.schemaBodyJson;
/**
 * @since 1.0.0
 * @category schema
 */
const schemaBodyForm = exports.schemaBodyForm = internal.schemaBodyForm;
/**
 * @since 1.0.0
 * @category schema
 */
const schemaBodyUrlParams = exports.schemaBodyUrlParams = internal.schemaBodyUrlParams;
/**
 * @since 1.0.0
 * @category schema
 */
const schemaBodyMultipart = exports.schemaBodyMultipart = internal.schemaBodyMultipart;
/**
 * @since 1.0.0
 * @category schema
 */
const schemaBodyFormJson = exports.schemaBodyFormJson = internal.schemaBodyFormJson;
/**
 * @since 1.0.0
 * @category conversions
 */
const fromWeb = exports.fromWeb = internal.fromWeb;
/**
 * @since 1.0.0
 * @category conversions
 */
const toURL = exports.toURL = internal.toURL;
//# sourceMappingURL=HttpServerRequest.js.map