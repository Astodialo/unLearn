"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.value = exports.unsafeWipe = exports.make = exports.isRedacted = exports.getEquivalence = exports.RedactedTypeId = void 0;
var Equivalence = _interopRequireWildcard(require("./Equivalence.js"));
var redacted_ = _interopRequireWildcard(require("./internal/redacted.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 3.3.0
 * @category symbols
 */
const RedactedTypeId = exports.RedactedTypeId = redacted_.RedactedTypeId;
/**
 * @since 3.3.0
 * @category refinements
 */
const isRedacted = exports.isRedacted = redacted_.isRedacted;
/**
 * This function creates a `Redacted<A>` instance from a given value `A`,
 * securely hiding its content.
 *
 * @example
 * import { Redacted } from "effect"
 *
 * const API_KEY = Redacted.make("1234567890")
 *
 * @since 3.3.0
 * @category constructors
 */
const make = exports.make = redacted_.make;
/**
 * Retrieves the original value from a `Redacted` instance. Use this function
 * with caution, as it exposes the sensitive data.
 *
 * @example
 * import { Redacted } from "effect"
 *
 * const API_KEY = Redacted.make("1234567890")
 *
 * assert.equal(Redacted.value(API_KEY), "1234567890")
 *
 * @since 3.3.0
 * @category getters
 */
const value = exports.value = redacted_.value;
/**
 * Erases the underlying value of a `Redacted` instance, rendering it unusable.
 * This function is intended to ensure that sensitive data does not remain in
 * memory longer than necessary.
 *
 * @example
 * import { Redacted } from "effect"
 *
 * const API_KEY = Redacted.make("1234567890")
 *
 * assert.equal(Redacted.value(API_KEY), "1234567890")
 *
 * Redacted.unsafeWipe(API_KEY)
 *
 * assert.throws(() => Redacted.value(API_KEY), new Error("Unable to get redacted value"))
 *
 * @since 3.3.0
 * @category unsafe
 */
const unsafeWipe = exports.unsafeWipe = redacted_.unsafeWipe;
/**
 * Generates an equivalence relation for `Redacted<A>` values based on an
 * equivalence relation for the underlying values `A`. This function is useful
 * for comparing `Redacted` instances without exposing their contents.
 *
 * @example
 * import { Redacted, Equivalence } from "effect"
 *
 * const API_KEY1 = Redacted.make("1234567890")
 * const API_KEY2 = Redacted.make("1-34567890")
 * const API_KEY3 = Redacted.make("1234567890")
 *
 * const equivalence = Redacted.getEquivalence(Equivalence.string)
 *
 * assert.equal(equivalence(API_KEY1, API_KEY2), false)
 * assert.equal(equivalence(API_KEY1, API_KEY3), true)
 *
 * @category equivalence
 * @since 3.3.0
 */
const getEquivalence = isEquivalent => Equivalence.make((x, y) => isEquivalent(value(x), value(y)));
exports.getEquivalence = getEquivalence;
//# sourceMappingURL=Redacted.js.map