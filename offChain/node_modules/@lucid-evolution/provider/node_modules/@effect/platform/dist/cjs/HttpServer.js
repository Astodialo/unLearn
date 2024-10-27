"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.withLogAddress = exports.serveEffect = exports.serve = exports.make = exports.logAddress = exports.formatAddress = exports.addressWith = exports.addressFormattedWith = exports.TypeId = exports.HttpServer = void 0;
var internal = _interopRequireWildcard(require("./internal/httpServer.js"));
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
const HttpServer = exports.HttpServer = internal.serverTag;
/**
 * @since 1.0.0
 * @category constructors
 */
const make = exports.make = internal.make;
/**
 * @since 1.0.0
 * @category accessors
 */
const serve = exports.serve = internal.serve;
/**
 * @since 1.0.0
 * @category accessors
 */
const serveEffect = exports.serveEffect = internal.serveEffect;
/**
 * @since 1.0.0
 * @category address
 */
const formatAddress = exports.formatAddress = internal.formatAddress;
/**
 * @since 1.0.0
 * @category address
 */
const addressWith = exports.addressWith = internal.addressWith;
/**
 * @since 1.0.0
 * @category address
 */
const addressFormattedWith = exports.addressFormattedWith = internal.addressFormattedWith;
/**
 * @since 1.0.0
 * @category address
 */
const logAddress = exports.logAddress = internal.logAddress;
/**
 * @since 1.0.0
 * @category address
 */
const withLogAddress = exports.withLogAddress = internal.withLogAddress;
//# sourceMappingURL=HttpServer.js.map