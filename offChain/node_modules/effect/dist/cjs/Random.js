"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.shuffle = exports.randomWith = exports.nextRange = exports.nextIntBetween = exports.nextInt = exports.nextBoolean = exports.next = exports.make = exports.choice = exports.RandomTypeId = exports.Random = void 0;
var defaultServices = _interopRequireWildcard(require("./internal/defaultServices.js"));
var internal = _interopRequireWildcard(require("./internal/random.js"));
function _getRequireWildcardCache(e) { if ("function" != typeof WeakMap) return null; var r = new WeakMap(), t = new WeakMap(); return (_getRequireWildcardCache = function (e) { return e ? t : r; })(e); }
function _interopRequireWildcard(e, r) { if (!r && e && e.__esModule) return e; if (null === e || "object" != typeof e && "function" != typeof e) return { default: e }; var t = _getRequireWildcardCache(r); if (t && t.has(e)) return t.get(e); var n = { __proto__: null }, a = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var u in e) if ("default" !== u && {}.hasOwnProperty.call(e, u)) { var i = a ? Object.getOwnPropertyDescriptor(e, u) : null; i && (i.get || i.set) ? Object.defineProperty(n, u, i) : n[u] = e[u]; } return n.default = e, t && t.set(e, n), n; }
/**
 * @since 2.0.0
 * @category symbols
 */
const RandomTypeId = exports.RandomTypeId = internal.RandomTypeId;
/**
 * Returns the next numeric value from the pseudo-random number generator.
 *
 * @since 2.0.0
 * @category constructors
 */
const next = exports.next = defaultServices.next;
/**
 * Returns the next integer value from the pseudo-random number generator.
 *
 * @since 2.0.0
 * @category constructors
 */
const nextInt = exports.nextInt = defaultServices.nextInt;
/**
 * Returns the next boolean value from the pseudo-random number generator.
 *
 * @since 2.0.0
 * @category constructors
 */
const nextBoolean = exports.nextBoolean = defaultServices.nextBoolean;
/**
 * Returns the next numeric value in the specified range from the
 * pseudo-random number generator.
 *
 * @since 2.0.0
 * @category constructors
 */
const nextRange = exports.nextRange = defaultServices.nextRange;
/**
 * Returns the next integer value in the specified range from the
 * pseudo-random number generator.
 *
 * @since 2.0.0
 * @category constructors
 */
const nextIntBetween = exports.nextIntBetween = defaultServices.nextIntBetween;
/**
 * Uses the pseudo-random number generator to shuffle the specified iterable.
 *
 * @since 2.0.0
 * @category constructors
 */
const shuffle = exports.shuffle = defaultServices.shuffle;
/**
 * Get a random element from an iterable.
 *
 * @example
 * import { Effect, Random } from "effect"
 *
 * Effect.gen(function* () {
 *   const randomItem = yield* Random.choice([1, 2, 3])
 *   console.log(randomItem)
 * })
 *
 * @since 3.6.0
 * @category constructors
 */
const choice = exports.choice = defaultServices.choice;
/**
 * Retreives the `Random` service from the context and uses it to run the
 * specified workflow.
 *
 * @since 2.0.0
 * @category constructors
 */
const randomWith = exports.randomWith = defaultServices.randomWith;
/**
 * @since 2.0.0
 * @category context
 */
const Random = exports.Random = internal.randomTag;
/**
 * Constructs the `Random` service, seeding the pseudo-random number generator
 * with an hash of the specified seed.
 * This constructor is useful for generating predictable sequences of random values for specific use cases.
 *
 * Example uses:
 * - Generating random UI data for visual tests.
 * - Creating data that needs to change daily but remain the same throughout a single day, such as using a date as the seed.
 *
 * @param seed - The seed value used to initialize the generator.
 *
 * @example
 * import { Effect, Random } from "effect"
 *
 * const random1 = Random.make("myseed")
 * const random2 = Random.make("myseed")
 *
 * assert.equal(Effect.runSync(random1.next), Effect.runSync(random2.next))
 *
 * @since 3.5.0
 * @category constructors
 */
const make = exports.make = internal.make;
//# sourceMappingURL=Random.js.map