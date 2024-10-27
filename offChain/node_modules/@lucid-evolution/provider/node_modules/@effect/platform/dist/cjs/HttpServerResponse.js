"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.urlParams = exports.updateCookies = exports.unsafeSetCookies = exports.unsafeSetCookie = exports.unsafeJson = exports.uint8Array = exports.toWeb = exports.text = exports.stream = exports.setStatus = exports.setHeaders = exports.setHeader = exports.setCookies = exports.setCookie = exports.setBody = exports.schemaJson = exports.replaceCookies = exports.removeCookie = exports.raw = exports.json = exports.isServerResponse = exports.htmlStream = exports.html = exports.formData = exports.fileWeb = exports.file = exports.empty = exports.TypeId = void 0;
var internal = _interopRequireWildcard(require("./internal/httpServerResponse.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 * @category type ids
 */
const TypeId = exports.TypeId = /*#__PURE__*/Symbol.for("@effect/platform/HttpServerResponse");
/**
 * @since 1.0.0
 */
const isServerResponse = exports.isServerResponse = internal.isServerResponse;
/**
 * @since 1.0.0
 * @category constructors
 */
const empty = exports.empty = internal.empty;
/**
 * @since 1.0.0
 * @category constructors
 */
const uint8Array = exports.uint8Array = internal.uint8Array;
/**
 * @since 1.0.0
 * @category constructors
 */
const text = exports.text = internal.text;
/**
 * @since 1.0.0
 * @category constructors
 */
const html = exports.html = internal.html;
/**
 * @since 1.0.0
 * @category constructors
 */
const htmlStream = exports.htmlStream = internal.htmlStream;
/**
 * @since 1.0.0
 * @category constructors
 */
const json = exports.json = internal.json;
/**
 * @since 1.0.0
 * @category constructors
 */
const schemaJson = exports.schemaJson = internal.schemaJson;
/**
 * @since 1.0.0
 * @category constructors
 */
const unsafeJson = exports.unsafeJson = internal.unsafeJson;
/**
 * @since 1.0.0
 * @category constructors
 */
const urlParams = exports.urlParams = internal.urlParams;
/**
 * @since 1.0.0
 * @category constructors
 */
const raw = exports.raw = internal.raw;
/**
 * @since 1.0.0
 * @category constructors
 */
const formData = exports.formData = internal.formData;
/**
 * @since 1.0.0
 * @category constructors
 */
const stream = exports.stream = internal.stream;
/**
 * @since 1.0.0
 * @category constructors
 */
const file = exports.file = internal.file;
/**
 * @since 1.0.0
 * @category constructors
 */
const fileWeb = exports.fileWeb = internal.fileWeb;
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
const removeCookie = exports.removeCookie = internal.removeCookie;
/**
 * @since 1.0.0
 * @category combinators
 */
const replaceCookies = exports.replaceCookies = internal.replaceCookies;
/**
 * @since 1.0.0
 * @category combinators
 */
const setCookie = exports.setCookie = internal.setCookie;
/**
 * @since 1.0.0
 * @category combinators
 */
const unsafeSetCookie = exports.unsafeSetCookie = internal.unsafeSetCookie;
/**
 * @since 1.0.0
 * @category combinators
 */
const updateCookies = exports.updateCookies = internal.updateCookies;
/**
 * @since 1.0.0
 * @category combinators
 */
const setCookies = exports.setCookies = internal.setCookies;
/**
 * @since 1.0.0
 * @category combinators
 */
const unsafeSetCookies = exports.unsafeSetCookies = internal.unsafeSetCookies;
/**
 * @since 1.0.0
 * @category combinators
 */
const setBody = exports.setBody = internal.setBody;
/**
 * @since 1.0.0
 * @category combinators
 */
const setStatus = exports.setStatus = internal.setStatus;
/**
 * @since 1.0.0
 * @category conversions
 */
const toWeb = exports.toWeb = internal.toWeb;
//# sourceMappingURL=HttpServerResponse.js.map