"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.isRegExp = exports.escape = void 0;
var predicate = _interopRequireWildcard(require("./Predicate.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * This module provides utility functions for working with RegExp in TypeScript.
 *
 * @since 2.0.0
 */

/**
 * Tests if a value is a `RegExp`.
 *
 * @param input - The value to test.
 *
 * @example
 * import { RegExp } from "effect"
 *
 * assert.deepStrictEqual(RegExp.isRegExp(/a/), true)
 * assert.deepStrictEqual(RegExp.isRegExp("a"), false)
 *
 * @category guards
 * @since 3.9.0
 */
const isRegExp = exports.isRegExp = predicate.isRegExp;
/**
 * Escapes special characters in a regular expression pattern.
 *
 * @example
 * import { RegExp } from "effect"
 *
 * assert.deepStrictEqual(RegExp.escape("a*b"), "a\\*b")
 *
 * @since 2.0.0
 */
const escape = string => string.replace(/[/\\^$*+?.()|[\]{}]/g, "\\$&");
exports.escape = escape;
//# sourceMappingURL=RegExp.js.map