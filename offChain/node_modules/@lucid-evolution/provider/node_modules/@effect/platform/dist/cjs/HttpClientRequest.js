"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.urlParamsBody = exports.updateUrl = exports.unsafeJsonBody = exports.uint8ArrayBody = exports.textBody = exports.streamBody = exports.setUrlParams = exports.setUrlParam = exports.setUrl = exports.setMethod = exports.setHeaders = exports.setHeader = exports.setHash = exports.setBody = exports.schemaBody = exports.removeHash = exports.put = exports.prependUrl = exports.post = exports.patch = exports.options = exports.modify = exports.make = exports.jsonBody = exports.head = exports.get = exports.formDataBody = exports.fileWebBody = exports.fileBody = exports.del = exports.bearerToken = exports.basicAuth = exports.appendUrlParams = exports.appendUrlParam = exports.appendUrl = exports.acceptJson = exports.accept = exports.TypeId = void 0;
var internal = _interopRequireWildcard(require("./internal/httpClientRequest.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 * @category type ids
 */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("@effect/platform/HttpClientRequest");
/**
 * @since 1.0.0
 * @category constructors
 */
const make = exports.make = internal.make;
/**
 * @since 1.0.0
 * @category constructors
 */
const get = exports.get = internal.get;
/**
 * @since 1.0.0
 * @category constructors
 */
const post = exports.post = internal.post;
/**
 * @since 1.0.0
 * @category constructors
 */
const patch = exports.patch = internal.patch;
/**
 * @since 1.0.0
 * @category constructors
 */
const put = exports.put = internal.put;
/**
 * @since 1.0.0
 * @category constructors
 */
const del = exports.del = internal.del;
/**
 * @since 1.0.0
 * @category constructors
 */
const head = exports.head = internal.head;
/**
 * @since 1.0.0
 * @category constructors
 */
const options = exports.options = internal.options;
/**
 * @since 1.0.0
 * @category combinators
 */
const modify = exports.modify = internal.modify;
/**
 * @since 1.0.0
 * @category combinators
 */
const setMethod = exports.setMethod = internal.setMethod;
/**
 * @since 1.0.0
 * @category combinators
 */
const setHeader = exports.setHeader = internal.setHeader;
/**
 * @since 1.0.0
 * @category combinators
 */
const setHeaders = exports.setHeaders = internal.setHeaders;
/**
 * @since 1.0.0
 * @category combinators
 */
const basicAuth = exports.basicAuth = internal.basicAuth;
/**
 * @since 1.0.0
 * @category combinators
 */
const bearerToken = exports.bearerToken = internal.bearerToken;
/**
 * @since 1.0.0
 * @category combinators
 */
const accept = exports.accept = internal.accept;
/**
 * @since 1.0.0
 * @category combinators
 */
const acceptJson = exports.acceptJson = internal.acceptJson;
/**
 * @since 1.0.0
 * @category combinators
 */
const setUrl = exports.setUrl = internal.setUrl;
/**
 * @since 1.0.0
 * @category combinators
 */
const prependUrl = exports.prependUrl = internal.prependUrl;
/**
 * @since 1.0.0
 * @category combinators
 */
const appendUrl = exports.appendUrl = internal.appendUrl;
/**
 * @since 1.0.0
 * @category combinators
 */
const updateUrl = exports.updateUrl = internal.updateUrl;
/**
 * @since 1.0.0
 * @category combinators
 */
const setUrlParam = exports.setUrlParam = internal.setUrlParam;
/**
 * @since 1.0.0
 * @category combinators
 */
const setUrlParams = exports.setUrlParams = internal.setUrlParams;
/**
 * @since 1.0.0
 * @category combinators
 */
const appendUrlParam = exports.appendUrlParam = internal.appendUrlParam;
/**
 * @since 1.0.0
 * @category combinators
 */
const appendUrlParams = exports.appendUrlParams = internal.appendUrlParams;
/**
 * @since 1.0.0
 * @category combinators
 */
const setHash = exports.setHash = internal.setHash;
/**
 * @since 1.0.0
 * @category combinators
 */
const removeHash = exports.removeHash = internal.removeHash;
/**
 * @since 1.0.0
 * @category combinators
 */
const setBody = exports.setBody = internal.setBody;
/**
 * @since 1.0.0
 * @category combinators
 */
const uint8ArrayBody = exports.uint8ArrayBody = internal.uint8ArrayBody;
/**
 * @since 1.0.0
 * @category combinators
 */
const textBody = exports.textBody = internal.textBody;
/**
 * @since 1.0.0
 * @category combinators
 */
const jsonBody = exports.jsonBody = internal.jsonBody;
/**
 * @since 1.0.0
 * @category combinators
 */
const unsafeJsonBody = exports.unsafeJsonBody = internal.unsafeJsonBody;
/**
 * @since 1.0.0
 * @category combinators
 */
const schemaBody = exports.schemaBody = internal.schemaBody;
/**
 * @since 1.0.0
 * @category combinators
 */
const urlParamsBody = exports.urlParamsBody = internal.urlParamsBody;
/**
 * @since 1.0.0
 * @category combinators
 */
const formDataBody = exports.formDataBody = internal.formDataBody;
/**
 * @since 1.0.0
 * @category combinators
 */
const streamBody = exports.streamBody = internal.streamBody;
/**
 * @since 1.0.0
 * @category combinators
 */
const fileBody = exports.fileBody = internal.fileBody;
/**
 * @since 1.0.0
 * @category combinators
 */
const fileWebBody = exports.fileWebBody = internal.fileWebBody;
//# sourceMappingURL=HttpClientRequest.js.map