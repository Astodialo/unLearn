"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.make = exports.hostStartsWith = exports.hostRegex = exports.hostExact = exports.hostEndsWith = exports.headerStartsWith = exports.headerRegex = exports.headerExact = exports.headerEndsWith = exports.empty = exports.add = exports.TypeId = void 0;
var internal = _interopRequireWildcard(require("./internal/httpMultiplex.js"));
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
const empty = exports.empty = internal.empty;
/**
 * @since 1.0.0
 * @category constructors
 */
const make = exports.make = internal.make;
/**
 * @since 1.0.0
 * @category combinators
 */
const add = exports.add = internal.add;
/**
 * @since 1.0.0
 * @category combinators
 */
const headerExact = exports.headerExact = internal.headerExact;
/**
 * @since 1.0.0
 * @category combinators
 */
const headerRegex = exports.headerRegex = internal.headerRegex;
/**
 * @since 1.0.0
 * @category combinators
 */
const headerStartsWith = exports.headerStartsWith = internal.headerStartsWith;
/**
 * @since 1.0.0
 * @category combinators
 */
const headerEndsWith = exports.headerEndsWith = internal.headerEndsWith;
/**
 * @since 1.0.0
 * @category combinators
 */
const hostExact = exports.hostExact = internal.hostExact;
/**
 * @since 1.0.0
 * @category combinators
 */
const hostRegex = exports.hostRegex = internal.hostRegex;
/**
 * @since 1.0.0
 * @category combinators
 */
const hostStartsWith = exports.hostStartsWith = internal.hostStartsWith;
/**
 * @since 1.0.0
 * @category combinators
 */
const hostEndsWith = exports.hostEndsWith = internal.hostEndsWith;
//# sourceMappingURL=HttpMultiplex.js.map