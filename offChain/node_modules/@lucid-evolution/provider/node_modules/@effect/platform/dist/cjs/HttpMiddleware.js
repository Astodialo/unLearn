"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.xForwardedHeaders = exports.withTracerDisabledWhenEffect = exports.withTracerDisabledWhen = exports.withTracerDisabledForUrls = exports.withLoggerDisabled = exports.searchParamsParser = exports.make = exports.loggerDisabled = exports.logger = exports.currentTracerDisabledWhen = exports.cors = void 0;
var internal = _interopRequireWildcard(require("./internal/httpMiddleware.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 1.0.0
 * @category constructors
 */
const make = exports.make = internal.make;
/**
 * @since 1.0.0
 * @category constructors
 */
const logger = exports.logger = internal.logger;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const loggerDisabled = exports.loggerDisabled = internal.loggerDisabled;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const withLoggerDisabled = exports.withLoggerDisabled = internal.withLoggerDisabled;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const currentTracerDisabledWhen = exports.currentTracerDisabledWhen = internal.currentTracerDisabledWhen;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const withTracerDisabledWhen = exports.withTracerDisabledWhen = internal.withTracerDisabledWhen;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const withTracerDisabledWhenEffect = exports.withTracerDisabledWhenEffect = internal.withTracerDisabledWhenEffect;
/**
 * @since 1.0.0
 * @category fiber refs
 */
const withTracerDisabledForUrls = exports.withTracerDisabledForUrls = internal.withTracerDisabledForUrls;
/**
 * @since 1.0.0
 * @category constructors
 */
const xForwardedHeaders = exports.xForwardedHeaders = internal.xForwardedHeaders;
/**
 * @since 1.0.0
 * @category constructors
 */
const searchParamsParser = exports.searchParamsParser = internal.searchParamsParser;
/**
 * @since 1.0.0
 * @category constructors
 */
const cors = exports.cors = internal.cors;
//# sourceMappingURL=HttpMiddleware.js.map